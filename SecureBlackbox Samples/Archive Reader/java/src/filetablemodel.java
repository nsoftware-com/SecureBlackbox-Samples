import java.util.Vector;
import javax.swing.table.*;
import secureblackbox.*;

import static secureblackbox.Archivewriter.*;

public class filetablemodel extends AbstractTableModel {
	private static final long serialVersionUID = -4060741556725801933L;
	public static final int FILENAME_INDEX   = 0;
	public static final int CSIZE_INDEX = 1;
    public static final int SIZE_INDEX  = 2;
    public static final int SECURITY_INDEX = 3;
    
    protected String[] columnNames = {
    		"File name", 
    		"Compressed size", 
    		"Original size",
            "Security" };
    
    protected Vector<filedatarow> dataVector = new Vector<filedatarow>();

 	public filetablemodel(Archivereader reader, String currentPath) {
		if (currentPath.length() > 0)
		{
			dataVector.add(new filedatarow("..", null));
		}

		for (int i = 0; i < reader.getFiles().size(); i++)
		{
			ArchivedFile entry = reader.getFiles().item(i);

			if (currentPath.equalsIgnoreCase(entry.getFolder()))
			{
                String name = entry.getFileName();
                if (entry.getDirectory())
                {
                    dataVector.add(new filedatarow(name, 0, 0, "", entry));
                }
                else
                {
                    dataVector.add(new filedatarow(name, entry.getCompressedSize(), entry.getSize(), getSecurityString(entry), entry));
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
             case SECURITY_INDEX:
                 return record.Security;
             case CSIZE_INDEX:
                return record.CompressSize;
             case SIZE_INDEX:
                return record.Size;
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
             case SECURITY_INDEX:
                 record.Security = (String)value;
                 break;
             case CSIZE_INDEX:
                record.CompressSize = (Integer)value;
                break;
             case SIZE_INDEX:
                record.Size = (Integer)value;
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

    public ArchivedFile getFirstSelectedItem(int firstRow) {
        return (ArchivedFile)((filedatarow)dataVector.get(firstRow)).Tag;
    }
 }