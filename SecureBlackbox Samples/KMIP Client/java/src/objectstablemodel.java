import javax.swing.table.AbstractTableModel;
import java.util.Vector;

import secureblackbox.*;

public class objectstablemodel extends AbstractTableModel {

	private static final long serialVersionUID = 1L;
	public static final int UNID_INDEX = 0;
	public static final int OBJTYPE_INDEX = 1;
	public static final int ALGORITHM_INDEX = 2;
	public static final int LENGTH_INDEX = 3;
	public static final int ID_INDEX = 4;

	protected String columnNames[] = { "UniqueIdentifier", "Type", "Algorithm", "Length", "Id" };
	protected Vector<objectsdatarow> dataVector = new Vector<objectsdatarow>();

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
		objectsdatarow record = dataVector.get(row);
		switch(col){
			case UNID_INDEX:
				return record.UniqueIdentifier;
			case OBJTYPE_INDEX:
				return record.ObjType;
			case ALGORITHM_INDEX:
				return record.Algorithm;
			case LENGTH_INDEX:
				return record.Length;
			case ID_INDEX:
				return record.Id;
		}
		return new Object();
	}

	public KMIPObject getSelectedRowTag(int row) {
		return dataVector.get(row).Tag;
	}

	public String getSelectedRowObjectType(int row) {
		return dataVector.get(row).ObjType;
	}

	public void addItem(KMIPObject info) {
		dataVector.add(new objectsdatarow(info));
		fireTableRowsInserted(dataVector.size(), dataVector.size());
	}

	public void clear() {
		dataVector.clear();
		fireTableStructureChanged();
	}

}
