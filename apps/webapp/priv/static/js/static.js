/* TEST */

var template =  _.template("<b><%- company %></b> $<%-price %><br/>")
$(function(){
    var count = 0;
    var webSocket = $.simpleWebSocket({ url: 'ws://127.0.0.1:4001/ws/stock' });
    window.setInterval(function()
                       {
                           if(webSocket.isConnected()== false) {
                               $("#error").html("ERROR NO CONNECTION, count="+ count );

                           } else{
                               $("#error").html("");
                           }
                       }, 101);

    webSocket.listen(function(message) {
        $("div#"+message.stock).html(template(message))
        $("span#version").html(message.version)
        count = count+1
        $("div#count").html(count)
    })
});
