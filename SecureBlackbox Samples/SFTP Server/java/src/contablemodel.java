import java.util.*;
import javax.swing.table.AbstractTableModel;

public class contablemodel extends AbstractTableModel {
	private static final long serialVersionUID = 1L;
	public static final int REMOTE_INDEX = 0;
	public static final int USER_INDEX = 1;
	public static final int TUNNELS_INDEX = 2;
	public static final int START_TIME_INDEX = 3;

	protected Vector<condatarow> dataVector = new Vector<condatarow>();
	
    protected String[] columnNames = {
    		"Remote host", 
    		"User",
    		"Tunnels",
    		"Start time"
    };

	public int getColumnCount() {
		return columnNames.length;
	}

 	public String getColumnName(int column) {
        return columnNames[column];
    }

 	public Class<?> getColumnClass(int column) {
       	return String.class;
    }

	public int getRowCount() {
		return dataVector.size();
	}

    public void setValueAt(Object value, int row, int column) {
		condatarow record = (condatarow)dataVector.get(row);
           switch (column) {
               case REMOTE_INDEX:
                  record.RemoteHost = (String)value;
                  break;
               case USER_INDEX:
                   record.User = (String)value;
                   break;
               case TUNNELS_INDEX:
                   record.Tunnels = (String)value;
                   break;
               case START_TIME_INDEX:
                   record.StartTime = (String)value;
                   break;
               default:
                  System.out.println("invalid index");
           }
           fireTableCellUpdated(row, column);
       } 	
	
	public Object getValueAt(int row, int col) {
		condatarow record = (condatarow)dataVector.get(row);
        switch (col) {
	        case REMOTE_INDEX:
	        	return record.RemoteHost;
            case USER_INDEX:
            	return record.User;
            case TUNNELS_INDEX:
            	return record.Tunnels;
            case START_TIME_INDEX:
            	return record.StartTime;
			default:
				return null;
        }
	}

	public void addRow(Long sessionId, String remoteAddress, String username, String tunnels) {
    	dataVector.addElement(new condatarow(sessionId, remoteAddress, username, tunnels, new Date().toString()));
    	fireTableDataChanged();
	}

	public void updateRowSession(Long sessionId, String tunnels) {
		Vector<condatarow> result = new Vector<condatarow>();

		for (int i=0; i < dataVector.size(); i++) {
			if (sessionId == dataVector.get(i).SessionId) {
				dataVector.get(i).Tunnels = tunnels;
			}
			result.add(dataVector.get(i));
		}

		dataVector.clear();
		dataVector.addAll(result);
		fireTableDataChanged();
	}

	public void removeRowSession(Long sessionId) {
		Vector<condatarow> result = new Vector<condatarow>();

		for (int i=0; i < dataVector.size(); i++) {
			if (sessionId == dataVector.get(i).SessionId)
				continue;
			result.add(dataVector.get(i));
		}
		
		dataVector.clear();
		dataVector.addAll(result);
		fireTableDataChanged();
	}
}
