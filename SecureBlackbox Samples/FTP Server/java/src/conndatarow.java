public class conndatarow {

	public String Host;
	public String User;
	public String CurDir;
	public String CurOp;
	public String Progress;
	public long SessionId;
	
	public conndatarow(long sessionId, String host) {
		SessionId = sessionId;
		Host = host;
	}

	public conndatarow(long sessionId,
			String username, String homeDirectory, String getLastCommand,
			String progress) {
		SessionId = sessionId;
		User = username;
		CurDir = homeDirectory;
		CurOp = getLastCommand;
		Progress = progress;
	}
}
