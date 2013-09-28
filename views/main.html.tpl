<div ng-controller="mainCtrl" class="full-page">
    <div class="header">
        <h3>Silent Mobile Disco</h3>
    </div>

    <table class="meta">
        <tr>
            <td width="33.3%" class="l">
                <div ng-if="status == 'playing' || status == 'playing_final_song'">
                    <div ng-show="has_revealed" class="songtitle">
                        [[ title ]]
                    </div>
                    <button class="btn small" app-button ng-click="song_title()" ng-show="!has_revealed">reveal song</button>
                    <div ng-show="!getReady">
                        [[ playback.currentTime|as_time ]]&nbsp;♫
                    </div>
                    
                </div>
            </td>
            <td width="33.3%" class="m">
                <div ng-if="status == 'playing'">
                    <button class="btn small" app-button ng-click="song_end()">skip song</button>
                </div>
            </td>
            <td width="33.3%" class="r">
                <button class="btn small" app-button ng-click="logout()">logout</button><br />
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
                <p class="step"><span class="nr">4</span>Exchange codes with your dance partner to win points!</p>
                
                <div class="buttons">
                    <button class="btn full" app-button ng-click="start()">Start</button>
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
                    <p ng-show="!has_scored" class="step"><span class="nr">3</span>The dancing game is on. There is only one other grooving to the same tune. Find this dancer by showing your moves!</p>
                    
                    <div class="buttons" ng-show="!getReady">
                        <button class="btn full" app-button ng-show="!has_scored" ng-click="enter_code()">I found my dance partner</button>
                        <p class="statusmsg" ng-show="has_scored">Enjoy your dance with [[ connected_player.name ]]!!</p>
                    </div>

                    <p class="statusmsg" ng-show="getReady">Get ready...!!!</p>

                    <p class="statusmsg" ng-show="!getReady">
                    </p>
                    <p ng-show="has_scored" class="statusmsg big">
                        [[ title ]]
                    </p>

                </div>

                <div ng-if="enteringCode">

                    <p class="step"><span class="nr">4</span>Enter your partner's code to earn points for the both of you.</p>

                    
                    <p class="statusmsg">
                        Your code:&nbsp;&nbsp;<span class="big">[[ secret_code ]]</span><a class="cancel" href="javascript:;" ng-click="cancelEnterCode()">cancel</a>
                    </p>
                    
                    <form name="enterForm" novalidate ng-submit="submitEnterCode()">
                        <input type="number" id="enternumber" ng-model="enter.code" placeholder="Enter your partner's code…" autofocus required />
                        <div class="buttons">
                            <button class="btn full" app-button ng-disabled="!enterForm.$valid">Go</button>
                        </div>
                    </form>
                    
                </div>                
            </div>

            <div ng-switch-when="playing_final_song">
                <p class="step"><span class="nr">The disco is over…!</span</p>
                <p class="statusmsg">Please enjoy this final song, and come back any time!</p>
                <p class="statusmsg small">Book your own mobile silent disco on <u>www.silentmobiledisco.nl</u></p>
            </div>
            
        </div>

        <button class="btn small lights" app-button ng-show="status=='playing'" ng-click="toggle_lights()"><span ng-show="!lights">Lights on!</span><span ng-show="lights">Light off</span></button>
        <button class="btn small panic" app-button ng-click="panic()">Panic!</button>
        
    </div>
</div>
