<?php

namespace Ampersand;

use Ampersand\Misc\Config;
use Ampersand\IO\Importer;
use Ampersand\Transaction;
use Ampersand\Plugs\StorageInterface;
use Ampersand\Rule\Conjunct;
use Ampersand\Session;
use Ampersand\Core\Atom;
use Exception;
use Ampersand\Interfacing\InterfaceObject;
use Ampersand\Core\Concept;
use Ampersand\Role;
use Ampersand\Rule\RuleEngine;
use Ampersand\Log\Notifications;
use Ampersand\IO\JSONReader;
use Psr\Log\LoggerInterface;
use Ampersand\Log\Logger;

class AmpersandApp
{
    /**
     * Specifies the required version of the localsettings file that
     * @const float
     */
    const REQ_LOCALSETTINGS_VERSION = 1.6;

    /**
     *
     * @var \Psr\Log\LoggerInterface
     */
    protected $logger;

    /**
     * List with storages that are registered for this application
     * @var \Ampersand\Plugs\StorageInterface[] $storages
     */
    protected $storages = [];

    /**
     * The session between AmpersandApp and user
     *
     * @var Session
     */
    protected $session = null;

    /**
     * List of accessible interfaces for the user of this Ampersand application
     *
     * @var \Ampersand\Interfacing\InterfaceObject[] $accessibleInterfaces
     */
    protected $accessibleInterfaces = [];
    
    /**
     * List with rules that are maintained by the activated roles in this Ampersand application
     *
     * @var \Ampersand\Rule\Rule[] $rulesToMaintain
     */
    protected $rulesToMaintain = []; // rules that are maintained by active roles
    
    /**
     * Constructor
     *
     * @param \Ampersand\Plugs\StorageInterface $defaultPlug
     * @param \Psr\Log\LoggerInterface $logger
     */
    public function __construct(StorageInterface $defaultPlug, LoggerInterface $logger)
    {
        $this->logger = $logger;

        // Register storages
        $this->registerStorage($defaultPlug);

        // Initiate session
        $this->setSession();

        // Set accessible interfaces and rules to maintain
        $this->setInterfacesAndRules();
    }
    
    public function registerStorage(StorageInterface $storage)
    {
        $this->logger->debug("Add storage: " . $storage->getLabel());
        $this->storages[] = $storage;
    }

    protected function setSession()
    {
        $this->session = new Session(Logger::getLogger('SESSION'));
    }

    protected function setInterfacesAndRules()
    {
        // Add public interfaces
        $this->accessibleInterfaces = InterfaceObject::getPublicInterfaces();

        // Add interfaces and rules for all active session roles
        foreach ($this->getActiveRoles() as $roleAtom) {
            try {
                $role = Role::getRoleByName($roleAtom->id);
                $this->accessibleInterfaces = array_merge($this->accessibleInterfaces, $role->interfaces());
                $this->rulesToMaintain = array_merge($this->rulesToMaintain, $role->maintains());
            } catch (Exception $e) {
                $this->logger->debug("Actived role '{$roleAtom}', but role is not used/defined in &-script.");
            }
        }

        // Remove duplicates
        $this->accessibleInterfaces = array_unique($this->accessibleInterfaces);
        $this->rulesToMaintain = array_unique($this->rulesToMaintain);
    }

    /**
     * Get the session object for this instance of the ampersand application
     *
     * @return Session
     */
    public function getSession()
    {
        return $this->session;
    }

    /**
     * Get list of accessible interfaces for the user of this Ampersand application
     *
     * @return \Ampersand\Interfacing\InterfaceObject[]
     */
    public function getAccessibleInterfaces()
    {
        return $this->accessibleInterfaces;
    }

    /**
     * Get the rules that are maintained by the active roles of this Ampersand application
     *
     * @return \Ampersand\Rule\Rule[]
     */
    public function getRulesToMaintain()
    {
        return $this->rulesToMaintain;
    }

    /**
     * Login user and commit transaction
     *
     * @return void
     */
    public function login(Atom $account)
    {
        // Set sessionAccount
        $this->session->setSessionAccount($account);

        // Commit transaction (exec-engine kicks also in)
        Transaction::getCurrentTransaction()->close();

        // Set (new) interfaces and rules
        $this->setInterfacesAndRules();
    }

    /**
     * Logout user, destroy and reset session
     *
     * @return void
     */
    public function logout()
    {
        $this->session->reset();
        $this->setInterfacesAndRules();
    }

    /**
     * Function to reinstall the application. This includes database structure and load default population
     *
     * @param boolean $installDefaultPop specifies whether or not to install the default population
     * @return \Ampersand\Transaction in which application is reinstalled
     */
    public function reinstall($installDefaultPop = true): Transaction
    {
        $this->logger->info("Start application reinstall");

        foreach ($this->storages as $storage) {
            $storage->reinstallStorage();
        }

        // Clear atom cache
        foreach (Concept::getAllConcepts() as $cpt) {
            $cpt->clearAtomCache();
        }

        if ($installDefaultPop) {
            $this->logger->info("Install default population");

            $reader = new JSONReader();
            $reader->loadFile(Config::get('pathToGeneratedFiles') . 'populations.json');
            $importer = new Importer($reader, Logger::getLogger('IO'));
            $importer->importPopulation();
        } else {
            $this->logger->info("Skip default population");
        }

        // Close transaction
        $transaction = Transaction::getCurrentTransaction()->close();
        if ($transaction->isRolledBack()) {
            Logger::getUserLogger()->error("Initial installation does not satisfy invariant rules");
        }

        // Initial conjunct evaluation
        $this->logger->info("Initial evaluation of all conjuncts after application reinstallation");
        
        // Evaluate all conjunct and save cache
        foreach (Conjunct::getAllConjuncts() as $conj) {
            $conj->evaluate(true);
            $conj->saveCache();
        }

        $this->setSession(); // Initiate session again

        $this->logger->info("End application reinstall");

        return $transaction;
    }

    /**
     * (De)activate session roles
     *
     * @param array $roles
     * @return void
     */
    public function setActiveRoles(array $roles)
    {
        foreach ($roles as $role) {
            // Set sessionActiveRoles[SESSION*Role]
            $this->session->toggleActiveRole(Concept::makeRoleAtom($role->label), $role->active);
        }
        
        // Commit transaction (exec-engine kicks also in)
        Transaction::getCurrentTransaction()->close();

        $this->setInterfacesAndRules();
    }

    /**
     * Get allowed roles
     *
     * @return \Ampersand\Core\Atom[]
     */
    public function getAllowedRoles()
    {
        return $this->session->getSessionAllowedRoles();
    }

    /**
     * Get active roles
     *
     * @return \Ampersand\Core\Atom[]
     */
    public function getActiveRoles(): array
    {
        return $this->session->getSessionActiveRoles();
    }

    /**
     * Get session roles with their id, label and state (active or not)
     *
     * @return array
     */
    public function getSessionRoles(): array
    {
        $activeRoleIds = array_column($this->getActiveRoles(), 'id');
        
        return array_map(function (Atom $roleAtom) use ($activeRoleIds) {
            return (object) ['id' => $roleAtom->id
                            ,'label' => $roleAtom->getLabel()
                            ,'active' => in_array($roleAtom->id, $activeRoleIds)
                            ];
        }, $this->getAllowedRoles());
    }

    /**
     * Check if session has any of the provided roles
     *
     * @param string[]|null $roles
     * @return bool
     */
    public function hasRole(array $roles = null): bool
    {
        // If provided roles is null (i.e. NOT empty array), then true
        if (is_null($roles)) {
            return true;
        }

        // Check for allowed roles
        return array_reduce($this->getAllowedRoles(), function (bool $carry, Atom $role) use ($roles) {
            return in_array($role->id, $roles) || $carry;
        }, false);
    }

    /**
     * Check if session has any of the provided roles active
     *
     * @param string[]|null $roles
     * @return bool
     */
    public function hasActiveRole(array $roles = null): bool
    {
        // If provided roles is null (i.e. NOT empty array), then true
        if (is_null($roles)) {
            return true;
        }

        // Check for active roles
        return array_reduce($this->getActiveRoles(), function (bool $carry, Atom $role) use ($roles) {
            return in_array($role->id, $roles) || $carry;
        }, false);
    }

    /**
     * Get interfaces that are accessible in the current session to 'Read' a certain concept
     * @param \Ampersand\Core\Concept[] $concepts
     * @return \Ampersand\Interfacing\InterfaceObject[]
     */
    public function getInterfacesToReadConcepts($concepts)
    {
        return array_values(
            array_filter($this->accessibleInterfaces, function ($ifc) use ($concepts) {
                foreach ($concepts as $cpt) {
                    if ($ifc->srcConcept->hasSpecialization($cpt, true)
                        && $ifc->crudR()
                        && (!$ifc->crudC() or ($ifc->crudU() or $ifc->crudD()))
                        ) {
                        return true;
                    }
                }
                return false;
            })
        );
    }

    /**
     * Determine if provided concept is editable concept in one of the accessible interfaces in the current session
     * @param \Ampersand\Core\Concept $concept
     * @return boolean
     */
    public function isEditableConcept(Concept $concept)
    {
        return array_reduce($this->accessibleInterfaces, function ($carry, $ifc) use ($concept) {
            return ($carry || in_array($concept, $ifc->getEditableConcepts()));
        }, false);
    }
    
    /**
     * Determine if provided interface is accessible in the current session
     * @param \Ampersand\Interfacing\InterfaceObject $ifc
     * @return boolean
     */
    public function isAccessibleIfc(InterfaceObject $ifc)
    {
        return in_array($ifc, $this->accessibleInterfaces, true);
    }

    /**
     * Evaluate and signal violations for all rules that are maintained by the activated roles
     *
     * @return void
     */
    public function checkProcessRules()
    {
        $this->logger->debug("Checking process rules for active roles: " . implode(', ', array_column($this->getActiveRoles(), 'id')));
        
        // Check rules and signal notifications for all violations
        foreach (RuleEngine::checkRules($this->getRulesToMaintain(), true) as $violation) {
            Notifications::addSignal($violation);
        }
    }
}