public class condatarow {
	public Long SessionId;
	public String RemoteHost;
	public String User;
	public String Tunnels;
	public String StartTime;
	
	public condatarow(Long sessionId, String remoteHost, String user, String tunnels, String startTime) {
		SessionId = sessionId;
		RemoteHost = remoteHost;
		User = user;
		Tunnels = tunnels;
		StartTime = startTime;
	}
}
