<html>
    <head>
        <title>Silent Mobile Disco</title>
	    <meta name="viewport" content="width=device-width, initial-scale=1" /> 
        
        <link rel="stylesheet" href="http://code.jquery.com/mobile/1.3.2/jquery.mobile-1.3.2.min.css" />
        <script src="http://code.jquery.com/jquery-1.9.1.min.js"></script>
        <script src="http://code.jquery.com/mobile/1.3.2/jquery.mobile-1.3.2.min.js"></script>

        {% lib
            "js/angular.min.js"
            "js/smd.js"
        %}
        
    </head>
    <body>
        <div data-role="page">
            {% block content %}
            {% endblock %}
        </div>
    </body>
</html>

    
