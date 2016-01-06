(function(){
  'use strict';

  // Prepare the 'problems' module for subsequent registration of controllers and delegates
  angular.module('problems', [ 'ngMaterial' ], function($locationProvider) {
      $locationProvider.html5Mode(true);
    });


})();
