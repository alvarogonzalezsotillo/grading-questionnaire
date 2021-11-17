'use strict';


const videoSettings = Object.seal({
    _width: null,
    _height: null,
    get width(){
        return this._width;
    },
    set width(value){
        this._width = value;
    },
    get height(){
        return this._height;
    },
    set height(value){
        this._height = value;
    },
});

const elements = Object.seal({
    video: null,
    canvas: null,
    log: null,
});



function getFacingModes(){
    return ["user","environment","left","right"];
}



function getVideoInput(videoElement,facingMode){
    

    facingMode = facingMode || "environment";
    
    let constraints = {
        audio: false,
        video: {
            width: { ideal: 1920 },
            height: { ideal: 1080 },
            facingMode : "environment"
        }
    };


    navigator.mediaDevices.getUserMedia(constraints)
        .then(stream => {
            let settings = stream.getVideoTracks()[0].getSettings();
            videoSettings.width = settings.width;
            videoSettings.height = settings.height;
            videoElement.srcObject = stream;
            videoElement.play();
        })
        .catch(error => {
            console.log(error);
            document.write("error en getuserMedia:" + error );
        });
}


function isLandscape(){
    return window.matchMedia("(orientation: landscape)").matches;
}


function takePhoto(video,canvas){
    var context = canvas.getContext('2d');
    if( isLandscape() ){
        canvas.width = videoSettings.width;
        canvas.height = videoSettings.height;
    }
    else{
        canvas.width = videoSettings.height;
        canvas.height = videoSettings.width;
    }
    context.drawImage(video, 0, 0, canvas.width, canvas.height);

    //     var data = canvas.toDataURL('image/png');
    //     photo.setAttribute('src', data);
}


function main(){
    elements.video = document.getElementById("video");
    elements.canvas = document.getElementById("canvas");
    elements.log = document.getElementById("log");
    
    elements.video.addEventListener("click", () => takePhoto(elements.video,elements.canvas) );
    getVideoInput(elements.video);
}


window.addEventListener("load", main);

