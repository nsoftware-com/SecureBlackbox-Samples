import java.util.*;
import javax.swing.table.AbstractTableModel;

public class reqtablemodel extends AbstractTableModel {
	private static final long serialVersionUID = 1L;
	public static final int OPERATION_INDEX = 0;
	public static final int USER_INDEX = 1;

	protected Vector<reqdatarow> dataVector = new Vector<reqdatarow>();
	
    protected String[] columnNames = {
    		"Operation",
    		"User name"
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
		reqdatarow record = (reqdatarow)dataVector.get(row);
           switch (column) {
               case OPERATION_INDEX:
                  record.Operation = (String)value;
                  break;
               case USER_INDEX:
                   record.Username = (String)value;
                   break;
               default:
                  System.out.println("invalid index");
           }
           fireTableCellUpdated(row, column);
       } 	
	
	public Object getValueAt(int row, int col) {
		reqdatarow record = (reqdatarow)dataVector.get(row);
        switch (col) {
	        case OPERATION_INDEX:
	        	return record.Operation;
            case USER_INDEX:
            	return record.Username;
			default:
				return null;
        }
	}

	public void addRow(String operation, String username) {
    	dataVector.addElement(new reqdatarow(operation, username));
    	fireTableDataChanged();
	}
}
