"use strict";

exports.getAudioContext = function(){
        return new (window.AudioContext || window.webkitAudioContext)();
    };

exports.createGain = function(ctx){
    return function(){
        return ctx.createGain();
    };
};

exports.gainConnectToContextImpl = function (gain, ctx, unit){
    return function(){
        gain.connect(ctx.destination);
        return unit;
    };
};

exports.gainConnectToGainImpl = function(gain, destGain, unit){
    return function(){
        gain.connect(destGain);
        return unit;
    };
};

exports.createOscillator = function(ctx){
    return function(){
        return ctx.createOscillator();
    };
};

exports.oscillatorConnectToGainImpl = function(o, g, unit){
    return function(){
        o.connect(g);
        return unit;
    };
}

exports.startOscillatorImpl = function(n, o, unit){
    return function(){
        if(n == 0){
            o.start();
        } else{
            o.start(n);
        }
        return unit;
    };
};

exports.stopOscillatorImpl = function(n, o, unit){
    return function(){
        o.stop(n);
        return unit;
    };
};

exports.setOscillatorTypeImpl = function(o, type, unit){
    return function(){
        o.type = type;
        return unit;
    };
};

exports.setGainValueImpl = function(val, gain, unit){
    return function(){
        gain.gain.value = val;
        return unit;
    };
};

exports.getCurrentTime = function(ctx){
    return function(){
        return ctx.currentTime;
    };
};