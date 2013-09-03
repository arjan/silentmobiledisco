<div ng-controller="mainCtrl" class="full-page">
    <div class="header">
        <h3>Silent Mobile Disco</h3>
    </div>

    <table class="meta">
        <tr>
            <td width="33.3%" class="l">

            </td>
            <td width="33.3%" class="m">

            </td>
            <td width="33.3%" class="r">
                <button class="btn small" ng-click="logout()">logout</button><br />
                [[ name ]]<br />
                score: [[ score ]]<br />
                <span ng-show="online_count>0">players: [[ online_count ]]</span>
            </td>
        </tr>
    </table>
    
    <div class="content with-meta">
        
        <message-area></message-area>
        
        <div ng-switch="status">

            <div ng-switch-when="registered">
                <p class="step compact-space"><span class="nr">1</span>Click Start</p>
                <p class="step"><span class="nr">2</span>Wait for your dance partner…</p>
                <p class="step"><span class="nr">3</span>The dancing game is on. There is only one other dancer grooving to the same tune. Find this dancer by showing your moves.</p>
                <p class="step"><span class="nr">4</span>Exchange code with your dance partner to win points!</p>
                
                <div class="buttons">
                    <button class="btn full" ng-click="start()">Start</button>
                </div>
            </div>
            
            <div ng-switch-when="waiting">
                <p class="statusmsg">Waiting for someone else to join...</p>
                <p class="statusmsg"><img src="/lib/images/loading.gif" /></p>
                
            </div>

            <div ng-switch-when="buffering">
                <p ng-show="!bufferingDone" class="statusmsg">
                    Now loading the track...
                </p>
                <p ng-show="bufferingDone" class="statusmsg">
                    Waiting on your partner to finish loading the track...
                </p>
                <p class="statusmsg"><img src="/lib/images/loading.gif" /></p>
            </div>
            
            <div ng-switch-when="playing">
                <p>
                    <span class="right">My code: [[ secret_code ]]</span>
                    Let's disco!
                </p>

                <h2 ng-show="has_revealed">[[ title ]]</h2>

                <p ng-show="getReady">Get ready...!!!</p>
                <p ng-show="!getReady">[[ playback.currentTime|as_time ]] / [[ playback.duration|as_time ]]</p>

                <div class="buttons">
                    <button ng-show="!has_scored" ng-click="enter_code()">I found my dancing partner!</button>
                    <p ng-show="has_scored">Enjoy your dance with [[ connected_player.name ]]!!</p>
                    
                    <button class="secondary" ng-click="song_title()" ng-disabled="has_revealed">Reveal song title</button>
                    <button class="secondary" ng-click="song_end()">This song is boring…</button>
                </div>
            </div>
            
        </div>

        <button class="panic secondary" ng-click="panic()">Panic!</button>
        
    </div>
</div>
