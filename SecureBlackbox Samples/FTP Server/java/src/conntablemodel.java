import java.util.Vector;
import javax.swing.table.AbstractTableModel;

public class conntablemodel extends AbstractTableModel {

	private static final long serialVersionUID = 1L;
	public static final int HOST_INDEX = 0;
	public static final int USER_INDEX = 1;
	public static final int CURDIR_INDEX = 2;
	public static final int CUROP_INDEX = 3;
	public static final int PROGR_INDEX = 4;
	
	protected String columnNames[] = { "Remote host", "User", "Current directory", "Current operation", "Progress:" };
	protected Vector<conndatarow> dataVector = new Vector<conndatarow>();
	
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
		conndatarow record = dataVector.get(row);
		switch(col){
			case HOST_INDEX:
				return record.Host;
			case USER_INDEX:
				return record.User;
			case CURDIR_INDEX:
				return record.CurDir;
			case CUROP_INDEX:
				return record.CurOp;
			case PROGR_INDEX:
				return record.Progress;
		}
		return new Object();
	}

	public void clear() {
		dataVector.clear();
		fireTableStructureChanged();
	}

	public void remove(long sessionId)
	{		
		for (int i = 0; i < dataVector.size(); i++) 
		{
			if (dataVector.get(i).SessionId == sessionId)
			{
				dataVector.remove(i);
				fireTableDataChanged();
				return;
			}
		}		
	}

	public void change(long sessionId, String username, String homeDirectory, String getLastCommand, String progress) {
		for (int i = 0; i < dataVector.size(); i++)
		{
			conndatarow row = dataVector.get(i);
			if (row.SessionId == sessionId)
			{
				if (!username.equalsIgnoreCase("-"))
					row.User = username;
				if (!homeDirectory.equalsIgnoreCase("-"))
					row.CurDir = homeDirectory;
				if (!getLastCommand.equalsIgnoreCase("-"))
					row.CurOp = getLastCommand;
				if (!progress.equalsIgnoreCase("-"))
					row.Progress = progress;
				fireTableRowsUpdated(i, i);
				
				return;
			}
		}
	}

	public void add(long sessionId, String host)
	{
		dataVector.add(new conndatarow(sessionId, host));
		fireTableRowsInserted(dataVector.size(), dataVector.size());
	} 
}