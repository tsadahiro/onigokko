const { nowInSec, SkyWayAuthToken, SkyWayContext, SkyWayRoom, SkyWayStreamFactory, uuidV4 } = skyway_room;

// Elmアプリケーションを開始します
var app = Elm.Onigokko.init({
    node: document.getElementById('myapp')
});



const token = new SkyWayAuthToken({
  jti: uuidV4(),
  iat: nowInSec(),
  exp: nowInSec() + 60 * 60 * 24,
  scope: {
    app: {
	id: '489b7100-9ce2-4b50-ac74-1b143ef84667',
      turn: true,
      actions: ['read'],
      channels: [
        {
          id: '*',
          name: '*',
          actions: ['write'],
          members: [
            {
              id: '*',
              name: '*',
              actions: ['write'],
              publication: {
                actions: ['write'],
              },
              subscription: {
                actions: ['write'],
              },
            },
          ],

          sfuBots: [
            {
              actions: ['write'],
              forwardings: [
                {
                  actions: ['write'],
                },
              ],
            },
          ],
        },
      ],
    },
  },
}).encode('cwBzLkd/0a1vrqUzIegWO5Q09NAajW4CPF7qCCABJ/E=');



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


    const data = await SkyWayStreamFactory.createDataStream();
    console.log(app.ports);
    app.ports.moved.subscribe(function(p) {
	//console.log(p)
	data.write({class:"move",player:p});
    });
    app.ports.join.subscribe(async (room) => {
	console.log(room)
	if (room === '') return;

    const camera = new Camera(videoElement, {
	onFrame: async () => {
	    await hands.send({image: videoElement});
	},
	width: 600,
	height: 400
    });
    camera.start();
	const context = await SkyWayContext.Create(token);
	const channel = await SkyWayRoom.FindOrCreate(context, {
	    type: 'p2p',
	    name: room
	});
	if (channel.members.length >= 4) {
	    return;
	}
	const me = await channel.join();
	app.ports.skywayId.send({id:me.id,num:channel.members.length});
	await me.publish(data);

	const subscribeAndAttach = async (publication) => {
	    if (publication.publisher.id === me.id) return;

            const { stream } = await me.subscribe(publication.id);

            stream.onData.add((newdata) => {
		if (newdata.class == "move"){
		    app.ports.othersMove.send(newdata.player);
		}
		else if (newdata.class == "kingyo"){
		}
            })
	};
	channel.publications.forEach(subscribeAndAttach);
	channel.onStreamPublished.add((e) => subscribeAndAttach(e.publication));
    });
    
})();
