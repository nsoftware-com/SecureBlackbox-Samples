import java.util.Vector;
import javax.swing.table.*;
import secureblackbox.*;

import static secureblackbox.ArchivedFile.*;

public class filetablemodel extends AbstractTableModel {
	private static final long serialVersionUID = -4060741556725801933L;
	public static final int FILENAME_INDEX   = 0;
	public static final int CSIZE_INDEX = 1;
    public static final int SIZE_INDEX  = 2;
    public static final int SECURITY_INDEX = 3;
    public static final int ACTION_INDEX = 4;
    
    protected String[] columnNames = {
    		"File name", 
    		"Compressed size", 
    		"Original size",
            "Security",
    		"Action" };
    
    protected Vector<filedatarow> dataVector = new Vector<filedatarow>();

 	public filetablemodel(Archivewriter writer, String currentPath) {
		if (currentPath.length() > 0)
		{
			dataVector.add(new filedatarow("..", null));
		}

		for (int i = 0; i < writer.getFiles().size(); i++)
		{
            ArchivedFile entry = writer.getFiles().item(i);

			if (currentPath.equalsIgnoreCase(entry.getFolder()))
			{
                String name = entry.getFileName();
                if (entry.getDirectory())
                {
                    dataVector.add(new filedatarow(name, 0, 0, "", entry.getAction(), entry));
                }
                else
                {
                    dataVector.add(new filedatarow(name, entry.getCompressedSize(), entry.getSize(), getSecurityString(entry), entry.getAction(), entry));
                }
            }
		}
 	}

    private String getSecurityString(ArchivedFile entry) {
        String st = "";
        switch (entry.getEncryptionType())
        {
            case aetGeneric:
                st = "Encrypted (generic)";
                break;

            case aetWinZip:
                st = "Encrypted (WinZip " + entry.getEncryptionKeyLength() + ")";
                break;

            case aetStrong:
                st = "Encrypted (Strong-" + entry.getEncryptionAlgorithm() + "," + entry.getEncryptionKeyLength() + " bits)";
                break;
        }

        if (entry.getSigned())
            st = st + ", signed";

        return st;
    }

	public String getColumnName(int column) {
        return columnNames[column];
    }

 	public boolean isCellEditable(int row, int col) {
        return false;
    }
 	
 	public Class<?> getColumnClass(int column) {
         switch (column) {
         	 case FILENAME_INDEX:
             case SECURITY_INDEX:
         		 return String.class;
             case CSIZE_INDEX:
             case SIZE_INDEX:
             case ACTION_INDEX:
                return Integer.class;
             default:
                return Object.class;
         }
    }

    public Object getValueAt(int row, int col) {
         filedatarow record = (filedatarow)dataVector.get(row);
         switch (col) {
	         case FILENAME_INDEX:
	        	 return record.FileName;
             case CSIZE_INDEX:
                return record.CompressSize;
             case SIZE_INDEX:
                return record.Size;
             case SECURITY_INDEX:
                 return record.Security;
             case ACTION_INDEX:
                switch (record.Action)
                {
                    case atAdd:
                        return "Add";
                    case atKeep:
                        return "Keep";
                    case atUpdate:
                        return "Update";
                    case atDelete:
                        return "Delete";
                    default:
                        return "";
                }
             default:
                return new Object();
         }
    }

    public void setValueAt(Object value, int row, int column) {
         filedatarow record = (filedatarow)dataVector.get(row);
         switch (column) {
	         case FILENAME_INDEX:
	        	 record.FileName = (String)value;
	        	 break;
             case CSIZE_INDEX:
                record.CompressSize = (Integer)value;
                break;
             case SIZE_INDEX:
                record.Size = (Integer)value;
                break;
             case SECURITY_INDEX:
                 record.Security = (String)value;
                 break;
             case ACTION_INDEX:
                record.Action = (Integer)value;
                break;
             default:
                System.out.println("invalid index");
         }
         fireTableCellUpdated(row, column);
    }

    public int getRowCount() {
         return dataVector.size();
     }

    public int getColumnCount() {
         return columnNames.length;
     }

	public void removeSelectedItems(Archivewriter writer, int[] indexes) {
		Vector<filedatarow> result = new Vector<filedatarow>();

		for(int i = 0; i < dataVector.size(); i++)
		{
            filedatarow row = (filedatarow)dataVector.get(i);

		    if (selectedIndex(i, indexes))
            {
                ArchivedFile entry = (ArchivedFile)row.Tag;

                if (entry.getNewFile())
                {
                    writer.getFiles().remove(entry);
                    fireTableRowsDeleted(i, i);
                }
                else
                {
                    try {
                        entry.setAction(atDelete);
                        row.Action = atDelete;
                        result.add(row);
                    }
                    catch (Exception ex)
                    {}
                }
            }
			else
			{

				result.add(row);				
			}
		}
		dataVector.clear();
		dataVector.addAll(result);
	}

	private boolean selectedIndex(int i, int[] indexes) {
		for (int j = 0; j < indexes.length; j++)
			if (i == indexes[j]) return true;
		return false;
	}

	public ArchivedFile getFirstSelectedItem(int firstRow) {
		return (ArchivedFile)((filedatarow)dataVector.get(firstRow)).Tag;
	}
 }