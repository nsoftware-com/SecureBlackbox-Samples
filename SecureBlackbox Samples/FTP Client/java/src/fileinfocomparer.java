import java.util.Comparator;
import secureblackbox.*;

public class fileinfocomparer implements Comparator<FTPListEntry>{
	public int compare(FTPListEntry x, FTPListEntry y) {
		int ret = 0;
		if (((x.getFileType() == FTPListEntry.cfetDirectory) && (y.getFileType() == FTPListEntry.cfetDirectory)) ||
			((x.getFileType() != FTPListEntry.cfetDirectory) && (y.getFileType() != FTPListEntry.cfetDirectory)))
		{
			ret = x.getName().compareTo(y.getName());
		} 
		else 
		{
			if (x.getFileType() == FTPListEntry.cfetDirectory)
			{
				ret = -1;
			} 
			else 
			{
				ret = 1;
			}
		}			
		return ret;
	}

}
