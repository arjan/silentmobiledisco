<!DOCTYPE html>
<html ng-app="smd">
    <head>
        <title>Silent Mobile Disco</title>
	    <meta name="viewport" content="width=device-width, initial-scale=1" /> 

        {% lib
            "js/angular.min.js"
            "js/angular-mobile.js"
            "js/angular-jqm.js"
            "js/angular-zotonic.js"
            "js/smd.js"
        %}
        {% lib 
            "css/jquery.mobile.css"
            "css/smd.css"
        %}
    </head>

    <body>
        <app-init />
        <jqm-view></jqm-view>

        <audio id="player" style="height: 1px" />
    </body>

</html>
