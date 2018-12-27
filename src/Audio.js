"use strict";

exports.getAudioContext = function(){
    return function(){
        return new (window.AudioContext || window.webkitAudioContext)();
    };
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
        return this.ctx.createOscillator();
    };
};

exports.oscillatorConnectToGainImpl = function(o, g, unit){
    return function(){
        o.connect(g);
        return unit;
    };
}

exports.startOscillatorImpl = function(o, unit){
    return function(){
        o.start();
        return unit;
    };
};

exports.stopOscillatorImpl = function(o, unit){
    return function(){
        o.stop();
        return unit;
    };
};

exports.setGainValueImpl = function(val, gain, unit){
    return function(){
        gain.gain.value = val;
        return unit;
    };
};