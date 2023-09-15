import secureblackbox.UserAccount;

public class userdatarow {
	
	public String Name;
	public String HomeDir;
	public String SpeedLimit;
	
	public userdatarow(UserAccount tag) {
		Name = tag.getUsername();
		HomeDir = tag.getBasePath();
        if (tag.getIncomingSpeedLimit() > 0)
        	SpeedLimit = tag.getIncomingSpeedLimit() + " kb/sec";
        else
        	SpeedLimit = "unlimited";
	} 
}