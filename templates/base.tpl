<!DOCTYPE html>
<html ng-app="smd">
    <head>
        <title>Silent Mobile Disco</title>
	    <meta name="viewport" content="width=device-width, initial-scale=1" /> 

        {% lib
            "js/angular.min.js"
            "js/jquery.mobile.css.js"
            "js/angular-mobile.js"
            "js/angular-jqm.js"
            "js/smd.js"
        %}
        {% lib 
            "css/smd.css"
        %}
    </head>

    <body>
        <app-init player="{{ m.session.player }}" />
        <jqm-view></jqm-view>
    </body>

</html>

    
