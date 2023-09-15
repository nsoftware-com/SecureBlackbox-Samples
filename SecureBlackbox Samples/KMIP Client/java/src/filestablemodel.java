import javax.swing.table.AbstractTableModel;
import java.util.Vector;

import secureblackbox.*;

public class filestablemodel extends AbstractTableModel {

	private static final long serialVersionUID = 1L;
	public static final int NAME_INDEX = 0;
	public static final int SIZE_INDEX = 1;
	public static final int MODIF_INDEX = 2;
	public static final int OWNER_INDEX = 3;
	public static final int RIGHTS_INDEX = 4;

	protected String columnNames[] = { "Files", "Size", "Modified", "Owner", "Rights" };
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
			case MODIF_INDEX:
				return record.Modif;
			case OWNER_INDEX:
				return record.Owner;
			case RIGHTS_INDEX:
				return record.Rights;
		}
		return new Object();
	}

	public SFTPListEntry getSelectedRowTag(int row) {
		return dataVector.get(row).Tag;
	}

	public void addItem(SFTPListEntry info) {
		dataVector.add(new filesdatarow(info));
		fireTableRowsInserted(dataVector.size(), dataVector.size());
	}

	public void clear() {
		dataVector.clear();
		fireTableStructureChanged();
	}

}
