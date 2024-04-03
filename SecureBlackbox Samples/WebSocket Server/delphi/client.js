var msg = document.getElementById("message"); 
var sendBtn = document.getElementById("sendButton"); 
var msgList = document.getElementById("messageList"); 
if (typeof(WebSocket)=="undefined") { 
   alert("Your browser does not support WebSockets. Try to use the latest Chrome, Firefox or Safari."); 
} else { 
   // Use "wss" instead of "ws" for SSL
   var ws = new WebSocket("ws://" + location.host); 
   sendBtn.onclick = function() { 
      ws.send(msg.value); 
      msg.value = ""; 
   } 
   ws.onopen = function() {
      msgList.innerHTML = "CONNECTED<hr />" + msgList.innerHTML;
   }
   ws.onerror = function() {
      msgList.innerHTML = "ERROR<hr />" + msgList.innerHTML; 
   }
   ws.onmessage = function(event) { 
      msgList.innerHTML = event.data + "<hr />" + msgList.innerHTML; 
   } 
}