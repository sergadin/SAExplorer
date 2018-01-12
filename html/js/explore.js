"use strict";

$(function() {
    var ractive = Ractive({
        target: '#target',
        template: '#template',
        data: {
            greeting: 'Hello',
            name: 'world',
            ws_status: 'disconnected',
            key: 'value',
            progress_current_stage: 20,
            conferences: [],
        }
    });

    const wsUri = "ws://localhost:12345/";

    var websocket = new WebSocket(wsUri);
    var output = document.getElementById("output");

    websocket.onopen = evt => writeToScreen("CONNECTED");
    websocket.onclose = evt => writeToScreen("DISCONNECTED");
    websocket.onmessage = processMessage;
    websocket.onerror = evt => writeToScreen('<span style="color: red;">ERROR:</span> ' + evt.data);

    function processMessage(evt)
    {
        let data = JSON.parse(evt.data);
        if (data.hasOwnProperty("progress")) {
            ractive.set("progress_current_stage", data.progress * 100);
        } else if (data.hasOwnProperty("relevantConferences")) {
            ractive.set("conferences", data.relevantConferences.map(name => ({name: name})));
        }
        writeToScreen('<span style="color: blue;">RESPONSE: ' + evt.data + '</span>');
    }

    function writeToScreen(message)
    {
        var pre = document.createElement("p");
        pre.style.wordWrap = "break-word";
        pre.innerHTML = message;
        output.appendChild(pre);
    }

    if( typeof(WebSocket) != "function" ) {
        $('body').html("<h1>Error</h1><p>Your browser does not support HTML5 Web Sockets. Try Google Chrome instead.</p>");
    }

    $("#explore-btn").on('click', function(e) {
        e.preventDefault();
        var keywords_inp = $(this).closest('.input-group').find('#keywords')
        var keywords = keywords_inp.val()

        websocket.send(JSON.stringify({'keywords': keywords}));
    });

    function conferenceItemClicked(event) {
        let conferenceName = $(event.node).data("conference-name");
        $(event.node).addClass("active");
        websocket.send(JSON.stringify({'confname': conferenceName}));
    }

    ractive.on({
        itemClicked: conferenceItemClicked,
    });
});
