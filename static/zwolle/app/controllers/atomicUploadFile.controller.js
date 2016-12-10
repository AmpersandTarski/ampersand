angular.module('AmpersandApp').controller('AtomicUploadFileController', function($scope, $rootScope, FileUploader, NotificationService, RoleService){
    
    // File uploader stuff
    $scope.FileUploader = new FileUploader({
        alias : 'file', // fieldname as used in $_FILES['file']
        formData : [{'roleId[]' : RoleService.getActiveRoleIds()}], // the '[]' in param 'roleIds[]' is needed by the API to process it as array
        removeAfterUpload : true,
        autoUpload : true
    });
    
    $scope.FileUploader.onSuccessItem = function(fileItem, response, status, headers){
        NotificationService.updateNotifications(response.notifications);
        
        // Add response content (newly created FileObject) to ifc list in resource
        fileItem.resource[fileItem.ifc].push(response.content);
    };
    
    $scope.FileUploader.onErrorItem = function(item, response, status, headers){
        NotificationService.addError(response.error.message, response.error.code, true);
    };
});