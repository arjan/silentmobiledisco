<div ng-controller="registerCtrl" class="full-page">
    <div class="header">
        <h3>Silent Mobile Disco</h3>
    </div>

    <div class="content">
        <p>To join the Silent Mobile Disco, please start by entering your name</p>
        
        <form name="connectForm" novalidate>
            <input type="text" ng-model="name" placeholder="Enter your name..." autofocus required />
            <div class="buttons">
                <button ng-disabled="!connectForm.$valid" ng-click="start()">Continue</button>
            </div>
        </form>
    </div>
</div>
