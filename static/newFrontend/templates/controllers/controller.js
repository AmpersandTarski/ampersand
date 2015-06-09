/*
Controller for interface "$interfaceName$" (context: "$contextName$"). Generated code, edit with care.
Generated by $ampersandVersionStr$

INTERFACE "$interfaceName$" : $expAdl$ :: $source$ * $target$  ($if(!isRoot)$non-$endif$root interface)
Roles: [$roles;separator=", "$]
Editable relations: [$editableRelations;separator=", "$] 
*/

AmpersandApp.controller('$interfaceName$Controller', function (\$scope, \$rootScope, \$location, \$routeParams, Restangular, \$location, \$timeout) {
  
  \$scope.val = {};
  \$scope.initialVal = {};
  \$scope.showSaveButton = {}; // initialize object for show/hide save button
  \$scope.showCancelButton = {}; // initialize object for show/hide cancel button
  \$scope.resourceStatus = {}; // initialize object for resource status colors
  \$scope.myPromises = {}; // initialize object for promises, used by angular-busy module (loading indicator)
  
  if(typeof \$routeParams.resourceId != 'undefined'){
	  srcAtomId = \$routeParams.resourceId;
  }else{ 
	  srcAtomId = \$rootScope.session.id;
  }
	  
  // BaseURL to the API is already configured in AmpersandApp.js (i.e. 'http://pathToApp/api/v1/')
  srcAtom = Restangular.one('resource/$source$', srcAtomId);
  \$scope.val['$interfaceName$'] = new Array();
  
  // Only insert code below if interface is allowed to create new atoms. This is not specified in interfaces yet, so add by default
  if(\$routeParams['new']){
	\$scope.val['$interfaceName$'].post({}).then(function(data) { // POST
		\$rootScope.updateNotifications(data.notifications);
		\$scope.val['$interfaceName$'].push(Restangular.restangularizeElement(srcAtom, data.content, '$interfaceName$')); // Add to collection
		showHideButtons(data.invariantRulesHold, data.requestType, data.content.id);
	});
    
  // Else
  }else{
    srcAtom.all('$interfaceName$').getList().then(function(data){
	  \$scope.val['$interfaceName$'] = data;
    });
  }
  
  \$scope.\$on("\$locationChangeStart", function(event, next, current) { 
    console.log("location changing to:" + next);
    
    checkRequired = false; // default
    for(var item in \$scope.showSaveButton) { // iterate over all properties (resourceIds) in showSaveButton object
      if(\$scope.showSaveButton.hasOwnProperty( item ) ) { // only checks its own properties, not inherited ones
        if(\$scope.showSaveButton[item] == true) checkRequired = true; // if item is not saved, checkRequired before location change
      }
    }
    
    if(checkRequired){ // if checkRequired (see above)
    	confirmed = confirm("You have unsaved edits. Do you wish to leave?");
        if (event && !confirmed) { 
          event.preventDefault();
        }
    }
  });


  // The functions below are only necessary if the interface allows to add/delete the complete atom,
  // but since this cannot be specified yet in Ampersand we always include it.

  // Function to add a new Resource
  \$scope.addNewResource = function (){
    \$scope.val['$interfaceName$'].post({}).then(function(newItem) { // POST
      \$scope.val['$interfaceName$'].push(newItem); // Add to collection
    });
  }
  
  // Delete function to delete a complete Resource
  \$scope.deleteResource = function (resourceId){
    if(confirm('Are you sure?')){
      var resourceIndex = _getResourceIndex(resourceId, \$scope.val['$interfaceName$']);
      
      // myPromise is used for busy indicator
  	  \$scope.myPromises[resourceId] = new Array();
  	
      \$scope.myPromises[resourceId].push(\$scope.val['$interfaceName$'][resourceIndex]
        .remove({ 'requestType' : 'promise'})
        .then(function(data){
          \$rootScope.updateNotifications(data.notifications);
          \$scope.val['$interfaceName$'].splice(resourceIndex, 1); // remove from array
        }));
    }
  }

  // Put function to update a Resource
  \$scope.put = function(resourceId, requestType){
	
	var resourceIndex = _getResourceIndex(resourceId, \$scope.val['$interfaceName$']);
	
	requestType = requestType || 'feedback'; // set default requestType. This does not work if you want to pass in a falsey value i.e. false, null, undefined, 0 or ""
	
	// myPromise is used for busy indicator
	\$scope.myPromises[resourceId] = new Array();
	
	\$scope.myPromises[resourceId].push( \$scope.val['$interfaceName$'][resourceIndex]
      .put({'requestType' : requestType})
      .then(function(data) {
        \$rootScope.updateNotifications(data.notifications);
        \$scope.val['$interfaceName$'][resourceIndex] = \$.extend(\$scope.val['$interfaceName$'][resourceIndex], data.content);
        
        showHideButtons(data.invariantRulesHold, data.requestType, resourceId);
        
        if(data.invariantRulesHold && data.requestType == 'promise'){
        	// If this atom was updated with the 'new' interface, change url
			var location = \$location.search();
			if(location['new']){
				\$location.url('/$interfaceName$/' + data.content.id);
			}        	
        }
      }));
  }

  // Function to cancel edits and reset resource data
  \$scope.cancel = function(resourceId){
	  
	  var resourceIndex = _getResourceIndex(resourceId, \$scope.val['$interfaceName$']);
	  
	  // myPromise is used for busy indicator
	  \$scope.myPromises[resourceId] = new Array();
	  
	  \$scope.myPromises[resourceId].push(\$scope.val['$interfaceName$'][resourceIndex]
	  	.get()
	  	.then(function(data) {
	  		\$scope.val['$interfaceName$'][resourceIndex] = \$.extend(\$scope.val['$interfaceName$'][resourceIndex], data.plain());
	  		setResourceStatus(resourceId, 'default');
	  		\$scope.showSaveButton[resourceId] = false;
	  		\$scope.showCancelButton[resourceId] = false;
	  	}));
  }
  
$if(containsEditable)$  // The interface contains at least 1 editable relation

  // Function to patch only the changed attributes of a Resource
  \$scope.patch = function(resourceId){
	  var resourceIndex = _getResourceIndex(resourceId, \$scope.val['$interfaceName$']);
	  //patches = diff(\$scope.initialVal['$interfaceName$'][resourceIndex], \$scope.val['$interfaceName$'][resourceIndex]) // determine patches
	  console.log('not yet implemented');
  }
  
  $if(containsDATE)$  // The interface contains an editable relation to the primitive concept DATE
  // Function for Datepicker
  \$scope.datepicker = []; // empty array to administer if datepickers (can be multiple on one page) are open and closed
  \$scope.openDatepicker = function(\$event, datepicker) {
    \$event.preventDefault();
    \$event.stopPropagation();
  
    \$scope.datepicker[datepicker] = {'open' : true};
  }
  $else$  // The interface does not contain editable relations to primitive concept DATE
  $endif$

  // Function to add item to array of primitieve datatypes
  \$scope.addItem = function(obj, property, selected, resourceId){
    if(selected.value != ''){
      if(obj[property] === null) obj[property] = [];
      obj[property].push(selected.value);
      selected.value = '';
      \$scope.put(resourceId);
    }else{
    	console.log('Empty value selected');
    }
  }
  
  //Function to remove item from array of primitieve datatypes
  \$scope.removeItem = function(obj, key, resourceId){
    obj.splice(key, 1);
    \$scope.put(resourceId);
  }
$else$  // The interface does not contain any editable relations
$endif$
$if(containsEditableNonPrim)$  // The interface contains at least 1 editable relation to a non-primitive concept
  // AddObject function to add a new item (val) to a certain property (property) of an object (obj)
  // Also needed by addModal function.
  \$scope.addObject = function(obj, property, selected, resourceId){
    if(selected.id === undefined || selected.id == ''){
      console.log('selected id is undefined');
    }else{
      if(obj[property] === null) obj[property] = {};
      obj[property][selected.id] = {'id': selected.id};
      selected.id = ''; // reset input field
      \$scope.put(resourceId);
    }
  }
  
  // RemoveObject function to remove an item (key) from list (obj).
  \$scope.removeObject = function(obj, key, resourceId){
    delete obj[key];
    \$scope.put(resourceId);
  }
  
  // Typeahead functionality
  \$scope.typeahead = {}; // an empty object for typeahead


  // A property for every (non-primitive) tgtConcept of the editable relations in this interface
  $editableNonPrimTargets:{concept|\$scope.typeahead['$concept$'] = Restangular.all('resource/$concept$').getList().\$object;
  }$
$else$  // The interface does not contain editable relations to non-primitive concepts
$endif$

  function _getResourceIndex(itemId, items){
    var index;
    items.some(function(item, idx){
      return (item.id === itemId) && (index = idx)
    });
    return index;
  }
  
  //show/hide save button
  function showHideButtons(invariantRulesHold, requestType, resourceId){
	if(invariantRulesHold && requestType == 'feedback'){ // if invariant rules hold (promise is possible) and the previous request was not a request4feedback (i.e. not a request2promise itself)
	  \$scope.showSaveButton[resourceId] = true;
	  \$scope.showCancelButton[resourceId] = true;
	  setResourceStatus(resourceId, 'warning');
	}else if(invariantRulesHold && requestType == 'promise'){
	  \$scope.showSaveButton[resourceId] = false;
	  \$scope.showCancelButton[resourceId] = false;
	  setResourceStatus(resourceId, 'success');
	  \$timeout(function() {
	    setResourceStatus(resourceId, 'default');
	  }, 3000);
	}else{
	  setResourceStatus(resourceId, 'danger');
	  \$scope.showSaveButton[resourceId] = false;
	  \$scope.showCancelButton[resourceId] = true;
	}
  }
  
  function setResourceStatus(resourceId, status){
    \$scope.resourceStatus[resourceId] = {'warning' : false, 'danger' : false, 'default' : false, 'success' : false}; // set all to false
    \$scope.resourceStatus[resourceId][status] = true; // set new status    
  }
});