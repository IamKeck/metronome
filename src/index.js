const Elm = require("./main.elm");
const is_touchable = (function(){
    var ua = navigator.userAgent;
    if(ua.indexOf('iPhone') > 0 || ua.indexOf('iPod') > 0 || ua.indexOf('Android') > 0 && ua.indexOf('Mobile') > 0){
        return true;
    }else if(ua.indexOf('iPad') > 0 || ua.indexOf('Android') > 0){
        return true;
    }else{
        return false;
    }
})();

//タップにより画面が拡大されてしまうことを防ぐ
document.body.addEventListener("touchend", function(e){
    e.preventDefault();
});

const app = Elm.Main.fullscreen(is_touchable);


const audio = {
    ctx: new (window.AudioContext || window.webkitAudioContext)(),
    main_gain: null,
    head_note_gain: null,
    eighth_note_gain: null,
    sixteenth_note_gain: null,
    triplets_note_gain: null,
    lastSetHeadNote: 0,
    interval: 0,
    timer: null,

    initialize(){
        this.main_gain = this.ctx.createGain(),
        this.head_note_gain = this.ctx.createGain(),
        this.eighth_note_gain = this.ctx.createGain(),
        this.sixteenth_note_gain = this.ctx.createGain(),
        this.triplets_note_gain = this.ctx.createGain(),

        this.main_gain.gain.value = 1
        this.main_gain.connect(this.ctx.destination);
        this.head_note_gain.connect(this.main_gain);
        this.eighth_note_gain.connect(this.main_gain);
        this.sixteenth_note_gain.connect(this.main_gain);
        this.triplets_note_gain.connect(this.main_gain);
    },
    _setNote(when, connect_to){
        const length = 0.1;
        const oscillator = this.ctx.createOscillator();
        oscillator.connect(connect_to);
        oscillator.start(when);
        oscillator.stop(when + length);
    },
    setNotes(from){
        const until = this.ctx.currentTime + 2;
        const times = getRange(from, until, this.interval);
        this.lastSetHeadNote = times.length == 0 ? this.lastSetHeadNote : times.slice(-1)[0].head[0] || this.lastSetHeadNote;
        times.forEach((time)=>{
            time.head.forEach((time)=>this._setNote(time, this.head_note_gain));
            time.eighth.forEach((time)=>this._setNote(time, this.eighth_note_gain));
            time.sixteenth.forEach((time)=>this._setNote(time, this.sixteenth_note_gain));
            time.triplets.forEach((time)=>this._setNote(time, this.triplets_note_gain));
        });
    },
    setNewInterval(interval){
        this.interval = interval;
    },
    _start(){
        this.main_gain.gain.value = 1;
        this.setNotes(this.ctx.currentTime - this.interval); //最初の音は捨てられてしまうため
        this.timer = setInterval(()=>{
        this.setNotes(this.lastSetHeadNote);
        }, 2);
    },
    _stop(){
        this.main_gain.gain.value = 0;
        clearInterval(this.timer);
        this.timer = null;

    },
    toggle(){
        if(this.timer == null){
            this._start();
        } else{
            this._stop();
        }
    },
    setNewGains(new_gains){
        this.head_note_gain.gain.value = new_gains.headGain / 100;
        this.eighth_note_gain.gain.value = new_gains.eighthGain / 100;
        this.sixteenth_note_gain.gain.value = new_gains.sixteenthGain / 100;
        this.triplets_note_gain.gain.value = new_gains.tripletsGain / 100;
    }
};
audio.initialize();
app.ports.newInterval.subscribe((new_interval)=>{
    audio.setNewInterval(new_interval);
});
app.ports.toggle.subscribe((new_interval)=>{
    audio.setNewInterval(new_interval);
    audio.toggle();

});
app.ports.newGains.subscribe((new_gains)=>{
    audio.setNewGains(new_gains);

});
function getRange(start, stop, interval){
    const head_list = [];
    let current_time = start;
    const stop_time = stop - interval;
    while(current_time <= stop_time){
        head_list.push(current_time);
        current_time += interval;
    }
    const head_list_to_use = head_list.slice(1);
    return head_list_to_use.map((head_note)=>getBeatsInBar(head_note, interval));
}

function getBeatsInBar(head_note_time, interval){
    return {
        head: [head_note_time],
        eighth: [head_note_time + interval / 2],
        sixteenth : [head_note_time + interval / 4, head_note_time + interval / 4 * 3],
        triplets : [head_note_time + interval / 3, head_note_time + interval / 3 * 2]
    }
}
