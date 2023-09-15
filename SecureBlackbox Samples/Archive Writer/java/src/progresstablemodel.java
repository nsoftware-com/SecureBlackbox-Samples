import java.util.Date;
import java.util.Vector;
import javax.swing.table.AbstractTableModel;

public class progresstablemodel extends AbstractTableModel {
	private static final long serialVersionUID = 2771284071470299948L;
	public static final int TIME_INDEX = 0;
	public static final int MSG_INDEX = 1;
	
	protected Vector<progressdatarow> dataVector = new Vector<progressdatarow>();
	
    protected String[] columnNames = {
    		"Time", 
    		"Message", 
    };
    
    public void addRow(Date dateTime, String msg){
    	dataVector.addElement(new progressdatarow(dateTime, msg));
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
		progressdatarow record = (progressdatarow)dataVector.get(row);
        switch (col) {
	         case TIME_INDEX:
	        	 return record.DateTime;
            case MSG_INDEX:
               return record.Message;
            default:
               return new Object();
        }
    }

	public void clear() {
		dataVector.removeAllElements();
	}
}
