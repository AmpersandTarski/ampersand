<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Log;

use Monolog\Handler\HandlerInterface;
use Psr\Log\LoggerInterface;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class Logger
{
    
    /**
     * Contains all instantiated loggers
     * @var \Monolog\Logger[]
     */
    private static $loggers = [];
    
    /**
     * Contains list of handlers that are added to a logger when it is instantiated
     * @var \Monolog\Handler\AbstractHandler[]
     */
    private static $genericHandlers = [];
    
    /**
     * Associative array containing array with handlers for specific channels
     * @var array
     */
    private static $channelHandlers = [];
    
    /**
     *
     * @param string $channel
     * @return \Psr\Log\LoggerInterface
     */
    public static function getLogger(string $channel): LoggerInterface
    {
        
        if (isset(self::$loggers[$channel])) {
            return self::$loggers[$channel];
        } else {
            $logger = new \Monolog\Logger($channel);
            
            // Add generic handlers (i.e. for all channels)
            foreach (self::$genericHandlers as $handler) {
                $logger->pushHandler($handler);
            }
            
            // Add handlers for specific channels
            if (array_key_exists($channel, self::$channelHandlers)) {
                foreach (self::$channelHandlers[$channel] as $handler) {
                    $logger->pushHandler($handler);
                }
            }
            
            self::$loggers[$channel] = $logger;
            
            return $logger;
        }
    }
    
    /**
     *
     * @return \Psr\Log\LoggerInterface
     */
    public static function getUserLogger(): LoggerInterface
    {
        return Logger::getLogger('USERLOG');
    }
    
    /**
     * Register a handler that is added when certain logger (channel) is instantiated
     *
     * @param string $channel
     * @param \Monolog\Handler\HandlerInterface $handler
     * @return void
     */
    public static function registerHandlerForChannel(string $channel, HandlerInterface $handler)
    {
        self::$channelHandlers[$channel][] = $handler;
    }
    
    /**
     * Register a handler that is added to all loggers when instantiated
     *
     * @param \Monolog\Handler\HandlerInterface $handler
     * @return void
     */
    public static function registerGenericHandler(HandlerInterface $handler)
    {
        self::$genericHandlers[] = $handler;
    }
}
