import secureblackbox.UserAccount;

import java.util.Vector;
import javax.swing.table.AbstractTableModel;

public class usertablemodel extends AbstractTableModel {

    
	private static final long serialVersionUID = 1L;
    protected String[] columnNames = {"Username" };
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
        return record.Name;
	}

    public void setValueAt(String value, int row, int column) {
		userdatarow record = (userdatarow)dataVector.get(row);
        record.Name = value;
        fireTableCellUpdated(row, column);
    } 	
    
 	public void addRow(UserAccount user){
		dataVector.add(new userdatarow(user.getUsername()));
		fireTableDataChanged();
 	}

	public String getUserName(int selectedRow) {
		return dataVector.get(selectedRow).Name;
	}

	public void clear() {
		dataVector.clear();
	}
}
