function initialize() {
  initLogWindows();  // Cannot call this from the post callback in sendCommands, since the existing click events somehow
  initializeAtoms(); // cannot be unbound from there. Therefore, initialization is split in two functions: 
}                    // initialize and initializeAtoms (the latter also being called from sendCommands).

function initializeAtoms() {
  if ($('#AmpersandRoot').attr('editing') == 'true') {  
    setEditHandlers();
    traceDbCommands(); // to initialize command list
  }
  else
    setNavigationHandlers();
}

/* A clone of the top-level atom is parked on #Rollback at edit start. On cancel, the atom and its navigation handlers are put back 
 * on #ScrollPane. This is a feasible solution since the interfaces will be of a manageable size */
function startEditing() {
  $('#Rollback').empty(); // in case we start twice for some reason
  $('#Rollback').append($('#ScrollPane > .Atom').clone(true, true)); /* (true,true) is needed to deep-copy edit handlers */
  
  $('#AmpersandRoot').attr('editing','true');
  clearNavigationHandlers();
  setEditHandlers();
  traceDbCommands(); // to initialize command list

  clearLogItems($('#IssueList')); // lists are cleared here and in cancelEditing, in case back button causes multiple start or cancel actions
  clearLogItems($('#PhpLog'));
}

function cancelEditing() {
  window.onbeforeunload = null; // disable the navigation warning (it is set in computeDbCommands)
  //console.log("clearing warning");

  if ($('#AmpersandRoot').attr('isNew')=='true') {
    // If we cancel the creation of a new atom, the new atom is removed from the concept table
    // and we navigate back to the previous page. (We do need to create an atom on new, because then we 
    // can simply use Interface.php to edit it)
    
    var atom = $('#AmpersandRoot').attr('atom');
    var concept = $('#AmpersandRoot').attr('concept');
    $.post("php/Database.php",{ destroyAtom: atom, concept:concept },function receiveDataOnPost(data){
      console.log(data);
      history.go(-1);
    }) ;
    
  } else {
    $('#ScrollPane > .Atom').remove();  
    $('#ScrollPane').append($('#Rollback > .Atom')); // this constitutes a move
    
    clearLogItems($('#IssueList')); // lists are cleared here and in startEditing, in case back button causes multiple start or cancel actions
    clearLogItems($('#PhpLog'));
    
    $('#AmpersandRoot').attr('editing','false');
  }
}

function commitEditing() {
  if (getEmptyAtomsNotInTemplates().length > 0) {
    alert('Please fill out all <new> atoms first.');
    return;
  }
  if (getNonUniqueAtomLists().length > 0) {
    alert('Please resolve duplicate atom names.');
    return;
  }
  
  var dbCommands = computeDbCommands();
  //console.log("clearing warning");
  window.onbeforeunload = null; // disable the navigation warning (it is set in computeDbCommands)

  if ($('#AmpersandRoot').attr('isNew')=='true') {
    // If we commit a newly created atom, we also need to add the atom to the concept table.
    var atom = $('#AmpersandRoot').attr('atom');
    var concept = $('#AmpersandRoot').attr('concept');
    dbCommands.unshift( mkDbCommandAddToConcept(atom,concept) ); // put addToConcept command in front
  }
  sendCommands(dbCommands);
}

function sendCommands(commandArray) {
  $.post('php/Database.php',  
  { commands: JSON.stringify(commandArray), role: getSelectedRole() },
  function(data) {
    $results = $(data);
    $errors = $(data).find('.Error');
    $logMessages = $(data).find('.LogMsg');
    $ampersandErrors = $(data).find('.AmpersandErr');

    setLogItems($('#PhpLog'), $logMessages);
    
    if ($errors.length + $ampersandErrors.length > 0) {
      setLogItems($('#IssueList'), $ampersandErrors);
      addLogItems($('#IssueList'), $errors);
      $('#IssueList').attr('minimized','false'); // always maximize window
    } 
    else
      // after saving a new atom, we go back to the previous page
      // Staying on the page is awkward as the url has atom='' which causes creation of a new atom when reloaded.
      // This url also should not stay in the history, since back will then create a new atom. The problem can be solved
      // by using a more complicated way to create new atoms (using post instead of a simple link with atom='').
      if ($('#AmpersandRoot').attr('isNew')=='true')
        history.go(-1);
      else
        $.get(window.location.href, 
          function(data) {
            $newPage = $('<div>');
            $newPage.html(data);
        
            $('#ScrollPane > .Atom').remove();  
            $('#ScrollPane').append($newPage.find('#ScrollPane > .Atom'));

            $('#AmpersandRoot').attr('editing','false');

            initializeAtoms();   
        });
  });
}


function getEmptyAtomsNotInTemplates() {
  $emptyAtomsNotInTemplates = $('.Atom[atom=""]').map( function() {
    if ($(this).parents().filter('[rowType=NewAtomTemplate]').length)
      return null;
    else 
      return $(this);
  });
  return $emptyAtomsNotInTemplates;
}

function getNonUniqueAtomLists() {
  $nonUniqueAtomLists =$('.AtomList').map( function() { // called also on lists in templates, but that's not a big deal
    $atoms = $(this).find('>.AtomRow[rowType=Normal]>.AtomListElt>.Atom');
    for (var i=0; i<$atoms.length; i++)
      for (var j=i+1; j<$atoms.length; j++)
        if ($($atoms[i]).attr('atom') == $($atoms[j]).attr('atom'))
          return $(this); 
    return null;
  });
  return $nonUniqueAtomLists;
}

// Edit commands

function showRelation(relation, isFlipped) {
  return '<span style="font-family: Arial">'+relation+(isFlipped?'~':'')+'</span>';  
}

function showAtom(atom) {
  return atom ? atom : '<span style="color:red">EMPTY</span>';
}

function showDbCommand(dbCommand) {
  switch (dbCommand.dbCmd) {
    case 'update':
      var originalPair = '('+(dbCommand.parentOrChild == 'parent' ? dbCommand.originalAtom + ',' + dbCommand.childAtom 
                                                                  : dbCommand.parentAtom + ',' + dbCommand.originalAtom) + ')';
      var newPair = '('+showAtom(dbCommand.parentAtom)+','+showAtom(dbCommand.childAtom)+')';
      return 'Update in   '+ showRelation(dbCommand.relation,dbCommand.isFlipped) +': '+
                           (dbCommand.originalAtom =='' ? 'add ' : originalPair+' ~> ')+newPair;
    case 'delete':
      return 'Delete from '+showRelation(dbCommand.relation,dbCommand.isFlipped)+': ('+showAtom(dbCommand.parentAtom)+','+showAtom(dbCommand.childAtom)+')';
  }
  return 'Undefined command: '+dbCommand;
}


function mkDbCommandAddToConcept(atom, concept) {
  return {dbCmd: 'addToConcept', atom:atom, concept:concept};
}

// update with '' as originalAtom is insert
function mkDbCommandUpdate(relation, relationIsFlipped, parentAtom, childAtom, parentOrChild, originalAtom) {
  return {dbCmd: 'update', relation:relation, isFlipped:relationIsFlipped, parentAtom:parentAtom, childAtom:childAtom,
                           parentOrChild:parentOrChild, originalAtom:originalAtom};
}

function mkDbCommandDelete(relation, relationIsFlipped, parentAtom, childAtom) {
  return {dbCmd: 'delete', relation:relation, isFlipped:relationIsFlipped, parentAtom:parentAtom, childAtom:childAtom};
}

function computeDbCommands() {
  dbCommands = new Array();
  $('.Atom .Atom').map(function () { // for every parent child pair of atoms (only immediate, so no <parent> .. <atom>  .. <child>)
    $childAtom = $(this);
    if (getEnclosingAtomRow($childAtom).attr('rowType')!='NewAtomTemplate') {
      //console.log(getEnclosingAtom($childAtom).attr('atom') + '<-->' + $childAtom.attr('atom'));
      
      var $atomList = getEnclosingAtomList($childAtom);
      var relation = $atomList.attr('relation'); 
     
      if (relation) {
        var relationIsFlipped = $atomList.attr('relationIsFlipped') ? attrBoolValue($atomList.attr('relationIsFlipped')) : false;
        var $parentAtom = getEnclosingAtom($childAtom);
        var parentAtom = $parentAtom.attr('atom');
        var childAtom = $childAtom.attr('atom');

        // parent deleted does not affect child
        // if parent is new, then there will only be new children which are handled below. no need to do anything for parent
        
        if( $parentAtom.attr('status') == 'modified') {
           if ($childAtom.attr('status') != 'new' && $childAtom.attr('status') != 'deleted' ) {
              var originalAtom = $parentAtom.attr('originalAtom');
              var unmodifiedChildAtom = $childAtom.attr('originalatom') ?  $childAtom.attr('originalAtom') : childAtom;
              // we want to delete/update the original tuple with the original child, not a modified one
              dbCommands.push(mkDbCommandUpdate(relation, relationIsFlipped, parentAtom, unmodifiedChildAtom, 'parent', originalAtom));
            }
        }

        switch($childAtom.attr('status')) {
          case 'new':
            dbCommands.push(mkDbCommandUpdate(relation, relationIsFlipped, parentAtom, childAtom, 'child', ''));
            break;
          case 'deleted':
            var unmodifiedParentAtom = $parentAtom.attr('originalAtom') ?  $parentAtom.attr('originalAtom') : parentAtom;
            var unmodifiedChildAtom = $childAtom.attr('originalatom') ?  $childAtom.attr('originalAtom') : childAtom;
            dbCommands.push(mkDbCommandDelete(relation, relationIsFlipped, unmodifiedParentAtom, unmodifiedChildAtom));
            break;
          case 'modified':
            if ($parentAtom.attr('status') != 'new' && $childAtom.attr('status') != 'deleted' ) {
              var originalAtom = $childAtom.attr('originalAtom');
              // if parent is modified, the original tuple will already have been deleted (tree traversal handles parent first)
              // so we can update the tuple with the modified parent. Hence no special case for parent with status=modified
              dbCommands.push(mkDbCommandUpdate(relation, relationIsFlipped, parentAtom, childAtom, 'child', originalAtom));
              
            }
            break;
        }     
      }
    }
  });
  
  if (dbCommands.length > 0) { 
    //console.log("setting warning");
    window.onbeforeunload = function() { return "The page has been modified, are you sure you wish to navigate away?"; };
  } else {
    //console.log("clearing warning");
    window.onbeforeunload = null;
  }

  return dbCommands;
}

function traceDbCommands() {
  clearLogItems($('#DbCommandList'));
  var dbCmds = computeDbCommands();
  for (var i=0; i<dbCmds.length; i++)
    addLogItems($('#DbCommandList'), $('<div class=Command>'+showDbCommand(dbCmds[i])+'</div>'));
}



// Editing UI

function clearEditHandlers() {
  $('#AmpersandRoot .AtomList').unbind('mouseenter mouseleave');
  $('#AmpersandRoot .AtomName').unbind('click');
  $('#AmpersandRoot .DeleteStub').unbind('click');
  $('#AmpersandRoot .InsertStub').unbind('click');
}

function setEditHandlers() {
  setEditHandlersBelow($('#AmpersandRoot'));
}

function setEditHandlersBelow($elt) {

  //on hover over an AtomList, set the attribute hover to true for that AtomList, but not for any child AtomLists
  // (this is not possible with normal css hover pseudo elements)
  $elt.find('.AtomList').hover(function () { 
    // mouse enter handler
    
    var $atomList = getEnclosingAtomList($(this));
    $atomList.attr('hover', 'false');
    $(this).attr('hover', 'true');
  }, function () {
    // mouse exit handler
      
    $atomList = getEnclosingAtomList($(this));
    $atomList.attr('hover', 'true');
    $(this).attr('hover', 'false');
  });
  
  $elt.find('.AtomName').click(function(){
    var $atomList = getEnclosingAtomList($(this));
    var relationToParent = $atomList.attr('relation');
    
    var $atom = getEnclosingAtom($(this)); 
    
    // this code also allows editing if there is a child atom in an editable relation
    //var childrenWithRelation = $atom.find('>.InterfaceList>.Interface>.AtomList[relation]');
    if (relationToParent /* || childrenWithRelation.length != 0 */) {
      startAtomEditing($atom);
    }
  });
 
  $elt.find('.DeleteStub').click(function() {
    var $atom = $(this).next().children().first(); // children is for AtomListElt

    if ($atom.attr('status')=='new')
      getEnclosingAtomRow($(this)).remove(); // remove the row of the table containing delete stub and atom
    else {
      if ($atom.attr('status') == 'modified') { // restore the original atom name on delete
        $atom.attr('atom', $atom.attr('originalAtom'));
        $atom.find('.AtomName').text($atom.attr('originalAtom')); // text() automatically escapes text
      }
      $atom.attr('status','deleted');
      getEnclosingAtomRow($(this)).attr('rowstatus','deleted'); // to make the entire row invisible
      $atom.find('.InterfaceList').remove(); // delete all interfaces below to prevent any updates on the children to be sent to the server
    }
    
    traceDbCommands();
  });
  
  $elt.find('.InsertStub').click(function (event) {
    var $atomList = getEnclosingAtomList($(this));
    
    $newAtomTemplate = $atomList.children().filter('[rowType=NewAtomTemplate]');
    
    $newAtomTableRow = $newAtomTemplate.clone();

    $newAtomTableRow.attr('rowType','Normal'); // remove the NewAtomTemplate class to make the new atom visible
    $newAtomTemplate.before( $newAtomTableRow ); 
    
    setEditHandlersBelow($newAtomTableRow); // add the necessary handlers to the new element
    // don't need to add navigation handlers, since page will be refreshed before navigating is allowed
    
    traceDbCommands();

    startAtomEditing($newAtomTableRow.find('>.AtomListElt>.Atom'));
  });
}

// Create a form that contains a text field, put it after $atom, and hide $atom.
function startAtomEditing($atom) {
  var $atomName = $atom.find('>.AtomName');
  var atom = $atom.attr('atom');
  if ($atom.attr('status')=='deleted')
    return;
      
  $textfield = $('<input type=text size=1 style="width:100%" value="'+atom+'"/>');
  $form = $('<form id=atomEditor style="margin:0px"/>'); // we use a form to catch the Return key event
  $form.append($textfield);
  $atomName.after($form);

  // stop editing when the textfield loses focus
  
  $textfield.focusout(function (obj) {
    // jQuery bug (or really bad feature): a select in the autocomplete menu triggers a blur event on the textfield.
    // If we stopEditing on this blur, the select action is not registered.
    // As a workaround, we don't stopEditing if the menu is visible. In that case, stopEditing is called in the 
    // autocomplete close event handler below. A consequence of this approach is that whenever the autocomplete menu is
    // closed, atom editing is also ended. However, this is not a big deal and also seems to be the only solution, as
    // there is no way to detect whether the blur was a genuine blur, or caused by select.
    if ($('.ui-autocomplete:visible').length == 0)
      stopAtomEditing($atom);
    
    return true;
  });
  // and when the user presses the return key
  $form.submit(function () {
    stopAtomEditing($atom);
    return false; // this prevents the browser from actually submitting the form
  });

  initializeAutocomplete($textfield, $atom);
  $textfield.focus().select();
  $atomName.hide();

}

// take the old value from $atom and replace its atom attribute as well as its text
// contents with the new value from the text field. Then show $atom again and
// remove the text field.
function stopAtomEditing($atom) {
  var $atomName = $atom.find('>.AtomName');
  var atom = $atom.attr('atom');
  
  var $form = $('#atomEditor');
  var newAtom = $form.children().filter('input').attr('value');
  $form.remove();

  $atom.attr('atom',newAtom);
  
  $atomName.text(newAtom);
  $atomName.attr('style',''); // Undo the hide() action from above. We don't use show, because that sets a style attribute on the div,
                              // which overrides all stylesheets
  
  if (newAtom!=atom) {
    
    if ($atom.attr('status')!='new') {
      
      if (!$atom.attr('originalAtom')) { // first time we edit this field, set originalAtom to the old value 
        $atom.attr('originalAtom',atom);
        $atom.attr('status','modified');
      } else                            // the atom was edited before
          if ($atom.attr('originalAtom')==newAtom)
            $atom.attr('status','unchanged'); // apparently, it was changed back to its original value
    }

    traceDbCommands();
  }
}

//The values are retrieved from the server with an getAtomsForConcept=<concept> request, so there is a slight
//delay before they are shown.
// For documentation, make sure to read docs.jquery.com/UI/Autocomplete and not docs.jquery.com/Plugins/autocomplete (which is incorrect)
function initializeAutocomplete($textfield, $atom) {
  var $atomList = getEnclosingAtomList($atom);
  var concept = $atomList.length ? $atomList.attr('concept') : $('#AmpersandRoot').attr('concept'); 
  // If there is no enclosing list, we are at the top-level atom and take the concept from AmpersandRoot 

  if (concept) {
    $.post("php/Database.php",{ getAtomsForConcept: concept },function receiveDataOnPost(data){
    var resultOrError = JSON.parse(data); // contains .res or .err
    if (typeof resultOrError.res !== 'undefined')
        $textfield.autocomplete({ source:resultOrError.res
                                , minLength: 0
                                , close: function(event, ui) { 
                                  stopAtomEditing($atom);
                                }
                                , select: function(event, ui) { 
                                    // Another jQuery problem: on a mouse click, the textfield has not been updated
                                    // at this point, so stopAtomEditing does not register the selected value.
                                    // As a workaround, we explicitly set the value to the menu selection.
                                    // Strangely enough, there is no problem for the Return key event.
                                    if (event.originalEvent && event.originalEvent.originalEvent &&
                                        event.originalEvent.originalEvent.type == "click")
                                      $textfield.attr('value', ui.item.value);

                                    stopAtomEditing($atom); 
                                    return true;
                                 }});
    else
        console.error("Ampersand: Error while retrieving auto-complete values:\n"+resultOrError.err);
    });
   } else
    console.error("Ampersand: Missing 'concept' html attribute for auto-complete");
}



//Navigation

function navigateTo(interface, atom) {
  window.location.href = "Interface.php?interface="+encodeURIComponent(interface)+"&atom="+encodeURIComponent(atom)
                                     +"&role="+getSelectedRole();     
}

function clearNavigationHandlers() {
  $('#AmpersandRoot .AtomName').unbind('click'); 
}

function setNavigationHandlers() {
  $("#AmpersandRoot .AtomName").map(function () {
    $atomList = getEnclosingAtomList($(this)); 
    $atom=getEnclosingAtom($(this));
    concept =$atomList.attr('concept');
    var atom = $atom.attr('atom');
    var interfaces = getInterfacesMap()[concept];  // NOTE: getInterfacesMap is assumed to be declared 
    // (since js has no import mechanism and we don't want to pass variables around all the time, a more elegant solution is not possible)
    
    if (typeof(interfaces) != 'undefined') { // if there are no interfaces for this concept, don't change the pointer and don't insert a click event
      $(this).click(function (event) {
        if (interfaces.length == 1)
          navigateTo(interfaces[0], atom);
        else
          mkInterfaceMenu(event, $(this), interfaces, atom);
      });
    }     
  });
}

function mkInterfaceMenu(event, $parentDiv, interfaces, atom) {
  $('#InterfaceContextMenu').remove();
  var $fullScreenMask = $("<div id=FullScreenMask/>");  /* the mask is for hiding the menu if we click anywhere outside it */
  var $menu = $('<div id=InterfaceContextMenu>');

  $fullScreenMask.click(function () {
    $menu.remove();
    $fullScreenMask.remove();
  });
  $('body').append($fullScreenMask);
  $parentDiv.append($menu);
  $menu.offset({ top: event.pageY, left: event.pageX });

  for (var i=0; i<interfaces.length; i++) {
    var $item = $('<div class=InterfaceContextMenuItem interface='+
                interfaces[i]+'>'+interfaces[i]+'</div>');   

    $menu = $menu.append($item);
    
    addClickEvent($item,interfaces[i],atom);
    // We need this separate function here to get a reference to i's value rather than the variable i.
    // (otherwise we encounter the 'infamous loop problem': all i's have the value of i after the loop)
  }
}

function addClickEvent($item, interface, atom) { 
  $item.click(function () {
    navigateTo(interface, atom);
    $('#InterfaceContextMenu').remove(); // so the menu + mask are gone when we press back
    $('#FullScreenMask').remove();
    return false;
  });
}



// LogWindows

function initLogWindows() {
  $('.LogWindow').click(function (event) {
    $(this).attr('minimized', $(this).attr('minimized')=='true' ? 'false' : 'true');
    return false;
  });  
}
function minimaximizeLogWindow(event) {
  console.log('Setting minimized to '+$(this).attr('minimized'));
  $(this).attr('minimized', $(this).attr('minimized')=='true' ? 'false' : 'true');
  return false;
}

function clearLogItems($logWindow) {
  $logWindow.find('.LogItem').remove();
  $logWindow.attr('nonEmpty','false');
}

// $logItems is a jQuery set of divs (LogItem class is added here)
function addLogItems($logWindow, $logItems) {
  $logItems.addClass('LogItem');
  $logWindow.append($logItems);
  if ($logItems.length > 0)
    $logWindow.attr('nonEmpty','true');
}

function setLogItems($logWindow, $logItems) {
  clearLogItems($logWindow);
  addLogItems($logWindow, $logItems);
}


// Roles

function changeRole() {
  console.log('Role changed to '+getSelectedRole());
  var interface = $('#AmpersandRoot').attr('interface');
  var atom = $('#AmpersandRoot').attr('atom');
  navigateTo(interface, atom); // navigate to takes the role from the updated selector
}


// Utils

function getEnclosingAtom($elt) {
  return $elt.parents().filter('.Atom').first();
}

function getEnclosingAtomList($elt) {
  return $elt.parents().filter('.AtomList').first();
}

function getEnclosingAtomRow($elt) {
  return $elt.parents().filter('.AtomRow').first();
}

function mapInsert(map, key, value) {
  if (map[key])
    map[key].push(value);
  else
    map[key] = [value];
}

function attrBoolValue(attrStr) {
  return attrStr.toLowerCase()=="true" ? true : false;
}

function getSelectedRole() {
  return $('#RoleSelector').val();
}

