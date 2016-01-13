(function(){

  angular
       .module('problems')
       .controller('ProblemController', [
          'problemService', '$mdSidenav', '$mdBottomSheet', '$log', '$q', '$http', '$location', '$window',
          ProblemController
       ]);

  /**
   * Main Controller for the Angular Material Starter App
   * @param $scope
   * @param $mdSidenav
   * @param avatarsService
   * @constructor
   */
  function ProblemController( problemService, $mdSidenav, $mdBottomSheet, $log, $q, $http, $location, $window) {
    var self = this;

    self.selected     = null;
    self.selectedDetails = null;
    self.problems        = [ ];
    self.problemFilters  = [ ];
    self.selectedProblemFilter = null;
    self.filteredProblems = [ ];

    self.loadingProblemToAnalyzer = false;

    self.selectProblem   = selectProblem;
    self.selectProblemFilter = selectProblemFilter;
    self.buildProblemFilters = buildProblemFilters;
    self.filterProblems = filterProblems;
    self.loadInAnalyzer = loadInAnalyzer;

    self.$location = $location

    // Load all registered problems

    problemService
          .loadAllProblems()
          .then( function success( problems ) {
            self.problems    = [].concat(problems);
            self.problemFilters = buildProblemFilters(self.problems)
            //self.selected = problems[0];
            self.selectProblemFilter(self.problemFilters[0]);
          },
          function fail(reason) {
            console.log("Failed to download", reason);
          });

    // *********************************
    // Internal methods
    // *********************************

    /**
     * Select the current avatars
     * @param menuId
     */
    function selectProblem ( problem ) {
      self.selected = angular.isNumber(problem) ? $scope.problems[problem] : problem

      problemService.loadProblemDetails(self.selected.problem_id).then(
        function successCallback(problemDetails) {
          self.selectedDetails = problemDetails
        },
        function failureCallback(reason) {

        }
      );
    }

    function selectProblemFilter ( problemFilter ) {
      self.selectedProblemFilter = angular.isNumber(problemFilter) ? $scope.problemFilters[problemFilter].problem_group : problemFilter.problem_group;
      self.filterProblems(self.selectedProblemFilter);
      self.selectProblem(self.filteredProblems[0]);
    }

    function filterProblems(newFilter) {
      self.filteredProblems = []
      self.problems.forEach(function(item) {
        if(item.problem_id.startsWith(newFilter + "_")) {
          self.filteredProblems.push(item);
        }
      });
    }

    function buildProblemFilters(problemList) {
      var filterMap = new Map();

      problemList.forEach(function(item) {
        var underscoreIndex = item.problem_id.indexOf("_");
        if(underscoreIndex != -1) {
          var problemGroup = item.problem_id.slice(0, underscoreIndex);

          var problemInfo = null;
          if(filterMap.has(problemGroup)) {
            var problemInfo = filterMap.get(problemGroup);
          } else {
            var problemInfo = {
              problem_group: problemGroup,
              problem_count: 0,
              iteration_count: 0
            }
          }
          problemInfo.problem_count++;
          problemInfo.iteration_count += item.iteration_count;

          filterMap.set(problemGroup, problemInfo);
        }
      });

      return [...filterMap.values()]; //transform back into array
    }

    function loadInAnalyzer(problemId) {
      self.loadingProblemToAnalyzer = true;
      return $http.jsonp($location.search().couchdb + '/_design/mdao/_list/as_csv/by_problem_id?key="' + problemId + '"&include_docs=true&callback=JSON_CALLBACK').then(
        function successCallback(response) {
          problemCsv = response.data.csv;

          //var fd = new FormData();
          //fd.append('file', problemCsv);
          $http.post("/csv", problemCsv, {
              transformRequest: angular.identity,
              headers: {'Content-Type': "text/csv"}
          })
          .success(function(response){
            console.log("Success! CSV is " + response.id);
            $window.open("/Dig/?csvfilename=" + response.id);
            self.loadingProblemToAnalyzer = false;
          })
          .error(function(){
            self.loadingProblemToAnalyzer = false;
          });
        }, 
        function errorCallback(reason) {
          //Forward rejection to next handler
          console.log("Download failed: ", reason);
          self.loadingProblemToAnalyzer = false;
          return $q.reject(reason)
        },
        function notifyCallback(update) {
          console.log("Update");
          console.log(update);
        }
      );
    }

  }

})();
