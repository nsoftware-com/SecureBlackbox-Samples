var msg = document.getElementById("message"); 
var sendBtn = document.getElementById("sendButton"); 
var msgList = document.getElementById("messageList"); 
if(typeof(WebSocket)=="undefined") { 
   alert("Your browser does not support WebSockets. Try to use the latest Chrome, Firefox or Safari."); 
} else { 
   var ws = new WebSocket("ws://" + location.host); 
   sendBtn.onclick = function() { 
      ws.send(msg.value); 
      msg.value = ""; 
   } 
   ws.onopen = function(event) {
      msgList.innerHTML = "CONNECTED: " + event.data+"<hr />" + msgList.innerHTML; 
   }
   ws.onerror = function(event) {
      msgList.innerHTML = "ERROR: " + event.data+"<hr />" + msgList.innerHTML; 
   }
   ws.onmessage = function(event) { 
      msgList.innerHTML = event.data + "<hr />" + msgList.innerHTML; 
   } 
}
