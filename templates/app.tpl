<!DOCTYPE html>
<html ng-app="smd">
    <head>
        <title>Silent Mobile Disco</title>
	    <meta name="viewport" content="width=device-width, initial-scale=1" /> 

        {% lib
            "js/apps/jquery-latest.min.js"
            "js/angular.min.js"
            "js/angular-zotonic.js"
            "js/smd.js"
        %}
        {% lib 
            "css/reset.css"
            "css/smd.css"
        %}
    </head>

    <body>
        <p class="unsupported" ng-show="!started">We are sorry, but it seems that your web browser cannot run the Silent Mobile Disco app… :-(</p>
        
        <app-init />
        <ng-view></ng-view>
    </body>

</html>
