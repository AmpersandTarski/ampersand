<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand\Rule;

use Ampersand\Misc\Config;
use Ampersand\Rule\Violation;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class RuleEngine
{

    /**
     * Function to get violations for a set of rules
     * Conjuncts are NOT re-evaluated
     *
     * @param \Ampersand\Rule\Rule[] $rules set of rules to check
     * @return \Ampersand\Rule\Violation[]
     */
    public static function getViolations(array $rules): array
    {
        // Evaluate rules
        $violations = [];
        foreach ($rules as $rule) {
            /** @var \Ampersand\Rule\Rule $rule */
            $violations = array_merge($violations, $rule->checkRule($forceReEvaluation = false));
        }
        return $violations;
    }
    
    /**
     * Get violations for set of rules from database cache
     *
     * @param \Ampersand\Rule\Rule[] $rules set of rules for which to query the violations
     * @return \Ampersand\Rule\Violation[]
     */
    public static function getViolationsFromCache(array $rules): array
    {
        global $container;
        /** @var \Psr\Cache\CacheItemPoolInterface $conjunctCache */
        $conjunctCache = $container['conjunctCachePool'];

        // Determine conjuncts to select from database
        $conjuncts = [];
        $conjunctRuleMap = []; // needed because violations are instantiated per rule (not per conjunct)
        foreach ($rules as $rule) {
            /** @var \Ampersand\Rule\Rule $rule */
            foreach ($rule->conjuncts as $conjunct) {
                $conjunctRuleMap[$conjunct->id][] = $rule;
            }
            $conjuncts = array_merge($conjuncts, $rule->conjuncts);
        }
        $conjuncts = array_unique($conjuncts); // remove duplicates
        
        if (empty($conjuncts)) {
            return [];
        }

        // Return violation
        $violations = [];
        foreach ($conjunctCache->getItems(array_column($conjuncts, 'id')) as $cacheItem) {
            /** @var \Psr\Cache\CacheItemInterface $cacheItem */
            $value = $cacheItem->get();
            foreach ($conjunctRuleMap[$value['conjId']] as $rule) {
                $violations[] = new Violation($rule, $value['src'], $value['tgt']);
            }
        }
        return $violations;
    }
}
