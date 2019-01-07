"use strict";

exports.elmSubscribeImpl = function(portName, elmApp, callback){
    return function(){
        if(elmApp.ports[portName] !== undefined){
            elmApp.ports[portName].subscribe(function(arg){
                callback(arg)();
            });
        }
    };
};

exports.elmDocumentImpl = function(flag, just, nothing){
    return function(){
        if(window.Elm){
            var app = Elm.Main.init({flags: flag});
            return just(app);
        }
        return nothing;
    };
};