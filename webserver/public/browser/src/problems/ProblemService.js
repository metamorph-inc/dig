(function(){
  'use strict';

  angular.module('problems')
         .service('problemService', ['$q', '$http', '$location', ProblemService]);

  /**
   * Problems DataService
   * Uses embedded, hard-coded data model; acts asynchronously to simulate
   * remote data service call(s).
   *
   * @returns {{loadAll: Function}}
   * @constructor
   */
  function ProblemService($q, $http, $location){
    return {
      loadAllProblems : function() {
        return $http.jsonp($location.search().couchdb + "/_design/mdao/_view/all_iterations?group=true&callback=JSON_CALLBACK").then(
          function successCallback(response) {
            //Transform response JSON into the format we need (array of objects with problem_id and iteration_count properties)
            var problems = []
            if(response.data.rows) {
              response.data.rows.forEach(function(problem) {
                problems.push({
                  problem_id: problem.key,
                  iteration_count: problem.value
                })
              });
              return problems
            } else {
              return $q.reject("Received unexpected/invalid response from CouchDB")
            }
          }, 
          function errorCallback(reason) {
            //Forward rejection to next handler
            console.log("Download failed: ", reason);
            return $q.reject(reason)
          }
        );
      },

      loadProblemDetails : function(problemId) {
        return $http.jsonp($location.search().couchdb + '/_design/mdao/_view/by_problem_id?key="' + problemId + '"&limit=1&include_docs=true&callback=JSON_CALLBACK').then(
          function successCallback(response) {
            //Returns a representative iteration from the specified problem set
            if(response.data.rows && response.data.rows[0] && response.data.rows[0].doc) {
              var problemDetails = {
                problem_id: response.data.rows[0].doc.problem_id
              }
              if(response.data.rows[0].doc.params) {
                problemDetails.params = Object.keys(response.data.rows[0].doc.params)
              } else {
                problemDetails.params = []
              }
              if(response.data.rows[0].doc.unknowns) {
                problemDetails.unknowns = Object.keys(response.data.rows[0].doc.unknowns)
              } else {
                problemDetails.unknowns = []
              }

              return problemDetails
            } else {
              return $q.reject("Received unexpected/invalid response from CouchDB")
            }
          }, 
          function errorCallback(reason) {
            //Forward rejection to next handler
            console.log("Download failed: ", reason);
            return $q.reject(reason)
          }
        );
      }
    };
  }

})();
