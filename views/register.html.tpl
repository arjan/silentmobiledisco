<div jqm-page ng-controller="registerCtrl">
    <div jqm-header>
        <h3>Sign in</h3>
    </div>

    <form name="connectForm" novalidate>
        <div jqm-textinput ng-model="player" placeholder="Enter your name..." required></div>
        <div class="buttons">
            <button ng-disabled="!connectForm.$valid" ng-click="start()">Click to start</button>
        </div>
    </form>

</div>
