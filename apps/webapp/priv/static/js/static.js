/* TEST */
var url = new URL('/ws/stock', window.location.href);

url.protocol = url.protocol.replace('http', 'ws');
console.log(url);

var template =  _.template("<b><%- company %></b> $<%-price %><br/>")
var setup = function(){
    console.info("SETUP");
    var count = 0;
    var webSocket = $.simpleWebSocket({ url: url.href });
    var interval = window.setInterval(function()
                       {
                           if(webSocket.isConnected()== false) {
                               $("#error")
                                   .html("ERROR NO CONNECTION" )
                                   .append($("<button>Restart</button>").click(setup));
                               clearInterval(interval);

                           } else{
                               $("#error").html("");
                           }
                       }, 101);

    webSocket.listen(function(message) {
        $("div#"+message.stock).html(template(message))
        $("span#version").html(message.version)
        count = count+1
    })
};
$(setup);
