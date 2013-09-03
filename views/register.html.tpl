<div ng-controller="registerCtrl" class="full-page">
    <div class="header">
        <h3>Silent Mobile Disco</h3>
    </div>

    <div class="content">
        
        <form name="connectForm" novalidate>
            <input type="text" ng-model="name" placeholder="Enter your (disco) name…" autofocus required />
            <div class="buttons">
                <button class="btn full" ng-disabled="!connectForm.$valid" ng-click="start()">Join disco</button>
            </div>
        </form>
    </div>
</div>
