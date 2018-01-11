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
        }
    })

    var wsUri = "ws://localhost:12345/";
    var output;
    var websocket;

    function init()
    {
        output = document.getElementById("output");
        testWebSocket();
    }

    function testWebSocket()
    {
        websocket = new WebSocket(wsUri);
        websocket.onopen = function(evt) { onOpen(evt) };
        websocket.onclose = function(evt) { onClose(evt) };
        websocket.onmessage = function(evt) { onMessage(evt) };
        websocket.onerror = function(evt) { onError(evt) };
        return websocket;
    }

    function onOpen(evt)
    {
        writeToScreen("CONNECTED");
        //doSend("WebSocket rocks");
    }

    function onClose(evt)
    {
        writeToScreen("DISCONNECTED");
    }

    function onMessage(evt)
    {
        writeToScreen('<span style="color: blue;">RESPONSE: ' + evt.data+'</span>');
        websocket.close();
    }

    function onError(evt)
    {
        writeToScreen('<span style="color: red;">ERROR:</span> ' + evt.data);
    }

    function doSend(message)
    {
        writeToScreen("SENT: " + message);
        websocket.send(JSON.stringify({'message': message}));
    }

    function writeToScreen(message)
    {
        var pre = document.createElement("p");
        pre.style.wordWrap = "break-word";
        pre.innerHTML = message;
        output.appendChild(pre);
    }

    window.addEventListener("load", init, false);



    if( typeof(WebSocket) != "function" ) {
        $('body').html("<h1>Error</h1><p>Your browser does not support HTML5 Web Sockets. Try Google Chrome instead.</p>");
    }

    $("#explore-btn").on('click', function(e) {
        e.preventDefault();
        var keywords_inp = $(this).closest('.input-group').find('#keywords')
        var keywords = keywords_inp.val()

        //var ws = testWebSocket()
        websocket.send(JSON.stringify({'keywords': keywords}))
        //ws.close();
    });

});
