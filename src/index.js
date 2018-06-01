const Elm = require("./main.elm");
const app = Elm.Main.fullscreen();

const audioCtx = new (window.AudioContext || window.webkitAudioContext)();
const main_gain = audioCtx.createGain();
main_gain.connect(audioCtx.destination);
const head_note_gain = audioCtx.createGain();
head_note_gain.gain.value = 1;
head_note_gain.connect(main_gain);
const eighth_note_gain = audioCtx.createGain();
eighth_note_gain.gain.value = 1;
eighth_note_gain.connect(main_gain);
sixteenth_note_gain = audioCtx.createGain();
sixteenth_note_gain.gain.value = 1;
sixteenth_note_gain.connect(main_gain);
triplets_note_gain = audioCtx.createGain();
triplets_note_gain.gain.value = 1;
triplets_note_gain.connect(main_gain);
let interval = null;
const setNote = (when, connect_to) =>{
    const length = 0.1;
    const oscillator = audioCtx.createOscillator();
    oscillator.connect(connect_to);
    oscillator.start(when);
    oscillator.stop(when + length);
};
const setNotes = (last_time) =>{
    const until = audioCtx.currentTime + 2;
    const times = getRange(last_time, until, interval);
    const new_last_time = times.length == 0 ? last_time : times.slice(-1)[0].head[0] || last_time;
    times.forEach((time)=>{
        time.head.forEach((time)=>setNote(time, head_note_gain));
        time.eighth.forEach((time)=>setNote(time, eighth_note_gain));
        time.sixteenth.forEach((time)=>setNote(time, sixteenth_note_gain));
        time.triplets.forEach((time)=>setNote(time, triplets_note_gain));
    });
    return new_last_time;

};
let timer = null;
app.ports.newInterval.subscribe((new_interval)=>{
    interval = new_interval;
});
app.ports.toggle.subscribe((new_interval)=>{
    interval = new_interval;
    if(timer == null){
        main_gain.gain.value = 1;
        let last_time = setNotes(audioCtx.currentTime - interval); //最初の音は捨てられてしまうため
        timer = setInterval(()=>{
            last_time = setNotes(last_time);
        }, 2);
    } else{
        main_gain.gain.value = 0;
        clearInterval(timer);
        timer = null;
    }
});
app.ports.newGains.subscribe((new_gains)=>{
    head_note_gain.gain.value = new_gains.headGain / 100;
    eighth_note_gain.gain.value = new_gains.eighthGain / 100;
    sixteenth_note_gain.gain.value = new_gains.sixteenthGain / 100;
    triplets_note_gain.gain.value = new_gains.tripletsGain / 100;

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