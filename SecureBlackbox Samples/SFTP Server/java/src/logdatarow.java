import java.util.Date;

public class logdatarow {
	public Date DateTime;
	public String Event;
	public String EFlag;
	
	public logdatarow(Date date, String event, String eFlag){
		DateTime = date;
		Event = event;
		EFlag = eFlag;
	}
}