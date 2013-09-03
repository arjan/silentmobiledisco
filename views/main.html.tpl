<div ng-controller="mainCtrl" class="full-page">
    <div class="header">
        <h3>Silent Mobile Disco</h3>
    </div>

    <table class="meta">
        <tr>
            <td width="33.3%" class="l">
                <div ng-if="status == 'playing'">
                    <span ng-show="has_revealed">[[ title ]]</span>
                    <button class="btn small" ng-click="song_title()" ng-show="!has_revealed">reveal song</button>
                </div>
            </td>
            <td width="33.3%" class="m">
                <div ng-if="status == 'playing'">
                    <button class="btn small" ng-click="song_end()">skip song</button>
                </div>
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
                <p class="step"><span class="nr">2</span>Wait for your dance partner…</p>
                <p class="statusmsg"><img src="/lib/images/loading.gif" /></p>
                <p class="statusmsg">You are waiting for someone to join</p>
                <p class="statusmsg small">Meanwhile you are listening to: mobile disco theme song</p>
            </div>

            <div ng-switch-when="buffering">
                <p class="step"><span class="nr">2</span>Joining the disco…</p>
                <p class="statusmsg"><img src="/lib/images/loading.gif" /></p>
                <p ng-show="!bufferingDone" class="statusmsg">
                    Now loading the track...
                </p>
                <p ng-show="bufferingDone" class="statusmsg">
                    Waiting on your partner to finish loading the track...
                </p>
            </div>
            
            <div ng-switch-when="playing">

                <div ng-if="!enteringCode">
                    <p class="step"><span class="nr">3</span>The dancing game is on. There is only one other grooving to the same tune. Find this dancer by showing your moves!</p>
                    
                    <p class="statusmsg" ng-show="getReady">Get ready...!!!</p>
                    <p class="statusmsg" ng-show="!getReady">
                        <img src="/lib/images/music.png" width="32" height="32" />
                        <br />
                        [[ playback.currentTime|as_time ]] / [[ playback.duration|as_time ]]
                    </p>

                    <div class="buttons space-above">
                        <button class="btn full" ng-show="!has_scored" ng-click="enter_code()">I found my dance partner</button>
                        <p ng-show="has_scored">Enjoy your dance with [[ connected_player.name ]]!!</p>
                    </div>
                </div>

                <div ng-if="enteringCode">
                    <p class="statusmsg">
                        You are code:<br />
                        <span class="big">[[ secret_code ]]</span>
                    </p>

                    <p class="step"><span class="nr">4</span>Enter your partner's code to earn points for the both of you.</p>

                    <form name="enterForm" novalidate ng-submit="submitEnterCode()">
                        <input type="text" id="enteredCode" placeholder="Enter your partner's code…" autofocus required />
                        <div class="buttons">
                            <button class="btn full" ng-disabled="!enterForm.$valid">Enter</button>
                            <button class="btn small" ng-click="cancelEnterCode()">cancel</button>
                        </div>
                    </form>
                    
                </div>                
            </div>
            
        </div>

        <button class="btn small panic" ng-click="panic()">Panic!</button>
        
    </div>
</div>
