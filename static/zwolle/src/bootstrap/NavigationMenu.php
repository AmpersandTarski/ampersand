<?php

use Ampersand\Misc\Config;
use Ampersand\AmpersandApp;

/**
 * @var \Pimple\Container $container
 */
global $container; // TODO: remove dependency to gloabl $container var

// Navigation menu settings
$angularApp = $container['angular_app'];
$angularApp->addMenuItem(
    'refresh',
    'app/src/admin/installer-menu-item.html',
    function (AmpersandApp $app) {
        return !Config::get('productionEnv');
    }
);

$angularApp->addMenuItem(
    'refresh',
    'app/src/admin/check-rules-menu-item.html',
    function (AmpersandApp $app) {
        return !Config::get('productionEnv');
    }
);

$angularApp->addMenuItem(
    'refresh',
    'app/src/admin/execengine-menu-item.html',
    function (AmpersandApp $app) {
        $roles = Config::get('allowedRolesForRunFunction', 'execEngine');
        return $app->hasActiveRole($roles);
    }
);

$angularApp->addMenuItem(
    'ext',
    'app/src/importer/menu-item.html',
    function (AmpersandApp $app) {
        $roles = Config::get('allowedRolesForImporter');
        return $app->hasActiveRole($roles);
    }
);

$angularApp->addMenuItem(
    'ext',
    'app/src/admin/exporter-menu-item.html',
    function (AmpersandApp $app) {
        return !Config::get('productionEnv');
    }
);