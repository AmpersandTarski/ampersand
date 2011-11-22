<?php
error_reporting(E_ALL^E_NOTICE); 
ini_set("display_errors", 1);

require "../Interfaces.php"; 
// defines $dbName, $isDev, $relationTableInfo, $allInterfaceObjects, and $rulesSql

require "DatabaseUtils.php";

echo '<div id="UpdateResults">';
//emitLog('ja');
//emitLog('ja');
//emitAmpersandErr('Rule was broken!');
//error('zaza');

dbStartTransaction($dbName);
emitLog('BEGIN');

processCommands();

if (checkRules()) {
  emitLog('COMMIT');
  dbCommitTransaction($dbName);
} else {
  emitLog('ROLLBACK');
  dbRollbackTransaction($dbName);
}

echo '</div>';

function processCommands() {  
  global $dbName; 
  $commandsJson =$_POST['commands']; 
  if (isset($commandsJson)) {
    $commandArray = json_decode($commandsJson);
          
    foreach ($commandArray as $command)
      processCommand($command);
       
  }
}

function processCommand($command) {
  if (!isset($command->dbCmd))
    error("Malformed command, missing 'dbCmd'");

  switch ($command->dbCmd) {
    case 'update':
      if (array_key_exists('relation', $command) && array_key_exists('isFlipped', $command) &&
          array_key_exists('parentAtom', $command) && array_key_exists('childAtom', $command) &&
          array_key_exists('parentOrChild', $command) && array_key_exists('originalAtom', $command))
        editUpdate($command->relation, $command->isFlipped, $command->parentAtom, $command->childAtom
                  ,$command->parentOrChild, $command->originalAtom);
      else 
        error("Command $command->dbCmd is missing parameters");
      break;
    case 'delete':
      if (array_key_exists('relation', $command) && array_key_exists('isFlipped', $command) &&
          array_key_exists('parentAtom', $command) && array_key_exists('childAtom', $command))
        editDelete($command->relation, $command->isFlipped, $command->parentAtom, $command->childAtom);
      else {
        error("Command $command->dbCmd is missing parameters");
      }
      break;
    default:
      error("Unknown command '$command->dbCmd'");
  }
}

function editUpdate($rel, $isFlipped, $parentAtom, $childAtom, $parentOrChild, $originalAtom) {
  if ($childAtom=='x') error('Don\'t update to \'x\'!');
  global $dbName;
  global $relationTableInfo;
  global $conceptTableInfo;
  global $tableColumnInfo;
  
  emitLog("editUpdate($rel, ".($isFlipped?'true':'false').", $parentAtom, $childAtom, $parentOrChild, $originalAtom)");
  
  $table = $relationTableInfo[$rel]['table'];
  $srcCol = $relationTableInfo[$rel]['srcCol'];
  $tgtCol = $relationTableInfo[$rel]['tgtCol'];
  $parentCol = $isFlipped ? $tgtCol : $srcCol;
  $childCol =  $isFlipped ? $srcCol : $tgtCol;
  
  $modifiedCol = $parentOrChild == 'parent' ? $parentCol : $childCol;
  $modifiedAtom= $parentOrChild == 'parent' ? $parentAtom : $childAtom;
  $stableCol   = $parentOrChild == 'parent' ? $childCol : $parentCol;
  $stableAtom  = $parentOrChild == 'parent' ? $childAtom: $parentAtom;
  
  $tableEsc = addSlashes($table);
  $modifiedColEsc = addSlashes($modifiedCol);
  $stableColEsc = addSlashes($stableCol);
  $modifiedAtomEsc = addSlashes($modifiedAtom);
  $stableAtomEsc = addSlashes($stableAtom);
  $originalAtomEsc = addSlashes($originalAtom);
  
  if ($tableColumnInfo[$table][$stableCol]['unique']) { // note: this uniqueness is not set as an SQL table attribute
    $query = "UPDATE `$tableEsc` SET `$modifiedColEsc`='$modifiedAtomEsc' WHERE `$stableColEsc`='$stableAtomEsc'";
    emitLog ($query);
    queryDb($dbName, $query);
  }
  else /* if ($tableColumnInfo[$table][$modifiedCol]['unique']) { // todo: is this ok? no, we'd also have to delete stableAtom originalAtom and check if modified atom even exists, otherwise we need an insert, not an update.
    $query = "UPDATE `$tableEsc` SET `$stableColEsc`='$stableAtomEsc' WHERE `$modifiedColEsc`='$modifiedAtomEsc'";
    emitLog ($query);
    queryDb($dbName, $query);
  }
  else */ {
    $query = "DELETE FROM `$tableEsc` WHERE `$stableColEsc`='$stableAtomEsc' AND `$modifiedColEsc`='$originalAtomEsc';";
    emitLog ($query);
    queryDb($dbName, $query);
    $query = "INSERT INTO `$tableEsc` (`$stableColEsc`, `$modifiedColEsc`) VALUES ('$stableAtomEsc', '$modifiedAtomEsc')";
    emitLog ($query);
    queryDb($dbName, $query);
  }
  // if the new atom is not in its concept table, we add it
  $childConcept = $isFlipped ? $relationTableInfo[$rel]['srcConcept'] : $relationTableInfo[$rel]['tgtConcept'];
  $parentConcept =  $isFlipped ? $relationTableInfo[$rel]['tgtConcept'] : $relationTableInfo[$rel]['srcConcept'];
  $modifiedConcept = $parentOrChild == 'parent' ? $parentConcept : $childConcept;
  
  $conceptTable = $conceptTableInfo[$modifiedConcept]['table'];
  $conceptCol = $conceptTableInfo[$modifiedConcept]['col'];
  
  $conceptTableEsc = addSlashes($conceptTable);
  $conceptColEsc = addSlashes($conceptCol);
  
  //emitLog("Checking existence of $childAtom : $childConcept in table $conceptTable, column $conceptCol";)
  $allConceptAtoms = firstCol(queryDb($dbName, "SELECT `$conceptColEsc` FROM `$conceptTableEsc`"));
  if (!in_array($modifiedAtom, $allConceptAtoms)) {
    //emitLog( 'not present');
    queryDb($dbName, "INSERT INTO `$conceptTableEsc` (`$conceptColEsc`) VALUES ('$modifiedAtomEsc')");
  } else {
    // emitLog('already present');
  }
}

function editDelete($rel, $isFlipped, $parentAtom, $childAtom) {
  if ($childAtom=='Pino') emitAmpersandErr('Don\'t delete Pino!');
  global $dbName; 
  global $relationTableInfo;
  emitLog ("editDelete($rel, ".($isFlipped?'true':'false').", $parentAtom, $childAtom)");
  $srcAtom = $isFlipped ? $childAtom : $parentAtom;
  $tgtAtom = $isFlipped ? $parentAtom : $childAtom;
  
  $table = $relationTableInfo[$rel]['table'];
  $srcCol = $relationTableInfo[$rel]['srcCol'];
  $tgtCol = $relationTableInfo[$rel]['tgtCol'];
  
  $tableEsc = addSlashes($table);
  $srcAtomEsc = addSlashes($srcAtom);
  $tgtAtomEsc = addSlashes($tgtAtom);
  $query = "DELETE FROM `$tableEsc` WHERE `$srcCol`='$srcAtomEsc' AND `$tgtCol`='$tgtAtomEsc';";
  emitLog ($query);
  queryDb($dbName, $query);
}

function checkRules() {
  global $rulesSql;
  
  $allRulesHold = true;
  
  foreach ($rulesSql as $ruleSql) {
    $rows = queryDb($dbName, $ruleSql['sql'], $error);
    if ($error) error($error);
    
    if (count($rows) > 0) {
      emitAmpersandErr("Rule '$ruleSql[name]' broken: $ruleSql[meaning]");
      $allRulesHold = false;
    }
    else
      emitLog('Rule '.$ruleSql['name'].' holds');
  } 
  return $allRulesHold;
}

function dbStartTransaction($dbName) {
  queryDb($dbName, 'START TRANSACTION');
}

function dbCommitTransaction($dbName) {
  queryDb($dbName, 'COMMIT');
}

function dbRollbackTransaction($dbName) {
  queryDb($dbName, 'ROLLBACK');
}

function queryDb($dbName, $querySql) {
  $result = DB_doquerErr($dbName, $querySql, $error);
  if ($error)
    error($error);
  
  return $result;
}

function emitAmpersandErr($err) {
  echo "<div class=AmpersandErr>$err</div>";
}

function emitLog($msg) {
  echo "<div class=LogMsg>$msg</div>";
}

function error($msg) {
  die("<div class=Error>Error in Database.php: $msg</div>");
} // because of this die, the top-level div is not closed, but that's better than continuing in an erroneous situtation
  // the current php session is broken off, which corresponds to a rollback. (doing an explicit roll back here is awkward
  // since it may trigger an error again, causing a loop)


?>