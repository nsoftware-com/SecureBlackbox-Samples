import java.util.Date;
import java.util.Vector;
import javax.swing.table.AbstractTableModel;


public class logtablemodel extends AbstractTableModel {
	private static final long serialVersionUID = 2771284071470299948L;
	public static final int E_INDEX = 0;
	public static final int TIME_INDEX = 1;
	public static final int MSG_INDEX = 2;
	
	protected Vector<logdatarow> dataVector = new Vector<logdatarow>();
	
    protected String[] columnNames = {
    		"Message Type",
    		"Time", 
    		"Event", 
    };
    
    public void addRow(Date dateTime, String msg, String eFlag){
    	dataVector.addElement(new logdatarow(dateTime, msg, eFlag));
    	fireTableDataChanged();
    }
    
	public String getColumnName(int column) {
        return columnNames[column];
    }
    
	public int getColumnCount() {
		return columnNames.length;
	}

	public int getRowCount() {
		return dataVector.size();
	}

    public Object getValueAt(int row, int col) {
		logdatarow record = (logdatarow)dataVector.get(row);
        switch (col) {
        	case E_INDEX:
        		return record.EFlag;
        	case TIME_INDEX:
	        	 return record.DateTime;
            case MSG_INDEX:
               return record.Event;
            default:
               return new Object();
        }
    }

	public void clear() {
		dataVector.removeAllElements();
	}
}