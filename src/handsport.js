var app = Elm.Hands.init({
    node: document.getElementById('myapp')
});

(async () => {
    function onResults(results) {
	canvasCtx.save();
	canvasCtx.clearRect(0, 0, canvasElement.width, canvasElement.height);
	canvasCtx.drawImage(
	    results.image, 0, 0, canvasElement.width, canvasElement.height);
	if (results.multiHandLandmarks) {
	    if (results.multiHandLandmarks.length == 2) {
		marks = results.multiHandLandmarks[0].concat(results.multiHandLandmarks[1]);
		app.ports.handsReceiver.send(marks);
		results.multiHandLandmarks.forEach(marks => {
		    for (const landmarks of results.multiHandLandmarks) {
			drawConnectors(canvasCtx, landmarks, HAND_CONNECTIONS,
				       {color: '#00FF00', lineWidth: 5});
			drawLandmarks(canvasCtx, landmarks, {color: '#FF0000', lineWidth: 2});
		    }
		});
	    }
	}
	canvasCtx.restore();
    }
    const videoElement = document.getElementsByClassName('input_video')[0];
    const canvasElement = document.getElementsByClassName('output_canvas')[0];
    const canvasCtx = canvasElement.getContext('2d');

    const hands = new Hands({locateFile: (file) => {
	return `https://cdn.jsdelivr.net/npm/@mediapipe/hands/${file}`;
    }});
    hands.setOptions({
	maxNumHands: 2,
	modelComplexity: 1,
	minDetectionConfidence: 0.5,
	minTrackingConfidence: 0.5
    });
    hands.onResults(onResults);
				       
    const camera = new Camera(videoElement, {
	onFrame: async () => {
	    await hands.send({image: videoElement});
	},
	width: 600,
	height: 400
    });
    camera.start();
})();
