import secureblackbox.UserAccount;

import java.util.Vector;
import javax.swing.table.AbstractTableModel;

public class usertablemodel extends AbstractTableModel {

	public static final int USER_INDEX = 0;
    public static final int HOME_INDEX = 1;
    public static final int SPEED_INDEX = 2;
    
	private static final long serialVersionUID = 1L;
    protected String[] columnNames = {"Username", "Home directory", "Speed limit" };
    protected Vector<userdatarow> dataVector = new Vector<userdatarow>();
    
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
        userdatarow record = (userdatarow)dataVector.get(row);
        switch (col) {
            case USER_INDEX:
               return record.Name;
            case HOME_INDEX:
               return record.HomeDir;
            case SPEED_INDEX:
                return record.SpeedLimit;
            default:
               return new Object();
        }
	}

 	public Class<?> getColumnClass(int column) {
        switch (column) {
            case USER_INDEX:
            case HOME_INDEX:
            case SPEED_INDEX:
            	return String.class;
            default:
               return Object.class;
        }
    }
 	
    public void setValueAt(Object value, int row, int column) {
        userdatarow record = (userdatarow)dataVector.get(row);
        switch (column) {
            case USER_INDEX:
               record.Name = (String)value;
               break;
            case HOME_INDEX:
               record.HomeDir = (String)value;
               break;
            case SPEED_INDEX:
                record.SpeedLimit = (String)value;
                break;               
            default:
               System.out.println("invalid index");
        }
        fireTableCellUpdated(row, column);
    } 	
    
 	public void addRow(UserAccount user){
		dataVector.add(new userdatarow(user));
		fireTableDataChanged();
 	}

	public void clear() {
		dataVector.clear();
		fireTableDataChanged();
	}
}