{% extends "base.tpl" %}

{% block content %}
    <div data-role="header">
        <h1>Silent Mobile Disco</h1>
    </div>

    <div ng-controller="main" >
        <gamedata player="{{ m.session.player }}" />
        
        <h1 ng-show="player">Hi, [[ player ]]</h1>
        
        <div ng-switch="status" data-role="content">

            <div ng-switch-when="disconnected">
                <form name="connectForm" novalidate>
                    <input type="text" ng-model="player" placeholder="Type your name..." required />
                    <span ng-show="connectForm.$valid">xx</span>
                    <button ng-show="connectForm.$valid" ng-click="connect()">Click to start</button>
                </form>
            </div>

            <div ng-switch-when="waiting">
                <p>Waiting for someone else to join...</p>
            </div>

            <div ng-switch-when="buffering">
                <p>Loading the track...</p>
            </div>
            
            <div ng-switch-when="playing">
                Now playing: [[ title ]]
                
                <button ng-click="song_end()">Song end</button>
            </div>
            
        </div>

        <audio id="player" style="height: 1px" />
        
    </div>
{% endblock %}
