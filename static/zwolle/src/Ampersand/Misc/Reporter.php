<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Misc;

use Exception;
use Ampersand\Interfacing\InterfaceObject;
use Ampersand\Misc\Config;
use Ampersand\IO\AbstractWriter;
use Ampersand\Rule\Conjunct;

class Reporter {

    /**
     * Writer 
     * 
     * @var \Ampersand\IO\AbstractWriter
     */
    protected $writer;

    public function __construct(AbstractWriter $writer){
        if(Config::get('productionEnv')) throw new Exception ("Reports are not allowed in production environment", 403);

        $this->writer = $writer;
    }

    public function __toString(){
        return $this->writer->getContent();
    }

    /**
     * Write and return interface report
     * 
     * @return array
     */
    public function reportInterfaceDefinitions(){
        $content = [];
        foreach (InterfaceObject::getAllInterfaces() as $key => $ifc) {
            $content = array_merge($content, $ifc->getInterfaceFlattened());
        }
        
        $content = array_map(function(InterfaceObject $ifc){
            return array( 'path' => $ifc->getPath()
                        , 'label' => $ifc->label
                        , 'crudC' => $ifc->crudC()
                        , 'crudR' => $ifc->crudR()
                        , 'crudU' => $ifc->crudU()
                        , 'crudD' => $ifc->crudD()
                        , 'src' => $ifc->srcConcept->name
                        , 'tgt' => $ifc->tgtConcept->name
                        , 'view' => $ifc->getView()->label
                        , 'relation' => $ifc->relation->signature
                        , 'flipped' => $ifc->relationIsFlipped
                        , 'ref' => $ifc->getRefToIfcId()
                        , 'root' => $ifc->isRoot()
                        , 'public' => $ifc->isPublic()
                        , 'roles' => implode(',', $ifc->ifcRoleNames)
                    );
            
        }, $content);

        $this->writer->write($content);

        return $content;
    }

    /**
     * Write and return conjunct usage report
     * 
     * Specifies which conjuncts are used by which rules, grouped by invariants, signals, and unused conjuncts
     *
     * @return array
     */
    public function reportConjunctUsage(){
        $content = [];
        foreach(Conjunct::getAllConjuncts() as $conj){        
            if($conj->isInvConj()) $content['invConjuncts'][] = $conj->__toString();
            if($conj->isSigConj()) $content['sigConjuncts'][] = $conj->__toString();
            if(!$conj->isInvConj() && !$conj->isSigConj()) $content['unused'][] = $conj->__toString();
        }

        $this->writer->write($content);

        return $content;
    }

}