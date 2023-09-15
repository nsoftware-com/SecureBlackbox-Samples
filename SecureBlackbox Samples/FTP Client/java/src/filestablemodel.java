import secureblackbox.*;
import javax.swing.table.AbstractTableModel;
import java.util.Vector;

public class filestablemodel extends AbstractTableModel {

	private static final long serialVersionUID = 1L;
	public static final int NAME_INDEX = 0;
	public static final int SIZE_INDEX = 1;
	public static final int DATE_INDEX = 2;
	public static final int TYPE_INDEX = 3;
	
	protected String columnNames[] = { "Files", "Size", "Date", "Type"};
	protected Vector<filesdatarow> dataVector = new Vector<filesdatarow>();
	
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
		filesdatarow record = dataVector.get(row);
		switch(col){
			case NAME_INDEX:
				return record.Name;
			case SIZE_INDEX:
				return record.Size;
			case DATE_INDEX:
				return record.FileDate;
			case TYPE_INDEX:
				return record.FileType;
		}
		return new Object();
	}

	public FTPListEntry getSelectedRowTag(int row) {
		return dataVector.get(row).Tag;
	}

	public void addItem(FTPListEntry info) {
		dataVector.add(new filesdatarow(info));
		fireTableRowsInserted(dataVector.size(), dataVector.size());
	}

	public void clear() {
		dataVector.clear();
		fireTableStructureChanged();
	}
 
}
