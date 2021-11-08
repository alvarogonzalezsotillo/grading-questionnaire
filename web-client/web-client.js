console.log("Empiezo web-client.js");


function getFacingModes(){
    return ["user","environment","left","right"];
}



function getVideoInput(videoElement,facingMode){
    

    function enumerateDevices(){
        navigator.mediaDevices.enumerateDevices().then(getVideoInputs).catch(errorCallback);

        function errorCallback(error){
            console.log(error);
        }
        
        function getVideoInputs(mediaDevices){
            let webcamList = [];
            mediaDevices.forEach(mediaDevice => {
                if (mediaDevice.kind === 'videoinput') {
                    webcamList.push(mediaDevice);
                }
            });

            console.log(webcamList);
        }
    }


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
            videoElement.srcObject = stream;
            videoElement.play();
        })
        .catch(error => {
            console.log(error);
            document.write("error en getuserMedia:" + error );
        });
}



function main(){
    let videoElement = document.getElementById("video");
    videoElement.addEventListener("click", takePhoto );
    getVideoInput(videoElement);
}

console.log("Acabo web-client.js");

window.addEventListener("load", main);

