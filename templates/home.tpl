{% extends "base.tpl" %}

{% block content %}
    <div data-role="header">
        <h1>Silent Mobile Disco</h1>
    </div>
    
    <div ng-app="smd">
        <div ng-controller="main">

            <div ng-switch="status" data-role="content">

                <div ng-switch-when="disconnected">
                    <button ng-click="connect()">Click to start</button>
                </div>

                <div ng-switch-when="waiting">
                    <p>Waiting for someone else to join...</p>
                </div>
                
                <div ng-switch-when="playing">
                    Now playing: [[ title ]]

                    
                    <button ng-click="song_end()">Song end</button>
                </div>
                
            </div>

            <div><audio id="player" controls /></div>
            
            {#            <audio id="player" style="height: 1px" />#}
            
        </div>
    </div>
{% endblock %}
