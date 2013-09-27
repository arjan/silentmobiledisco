<!DOCTYPE html>
<html ng-app="smd">
    <head>
        <title>Silent Mobile Disco</title>
        <meta name="viewport" content="user-scalable=no, initial-scale=1, maximum-scale=1, minimum-scale=1, width=device-width" />

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
        <div class="unsupported" ng-show="!started || unsupported">
            <div class="header">
                <h3>Silent Mobile Disco</h3>
            </div>
            <div class="content">
                <p><strong>We are sorry, but it seems that your web browser cannot run the Silent Mobile Disco app… :-(</strong></p>
                <p>
                    Your mobile browser needs to have support for WebSockets and for playing MP3 audio files.
                    Please try again using a different (mobile) web browser. Chrome is known to work.
                </p>
                <div class="buttons space-above">
                    <a class="btn full" href="http://www.google.com/chrome">Download Chrome</a>
                </div>
            </div>
        </div>
        
        <app-init />
        <ng-view></ng-view>
    </body>

</html>
