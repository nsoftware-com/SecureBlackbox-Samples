import java.util.Comparator;

import secureblackbox.*;

public class fileinfocomparer implements Comparator<SFTPListEntry>{
	public int compare(SFTPListEntry x, SFTPListEntry y) {
		int ret = 0;
		if (!((x.getDirectory()) ^ (y.getDirectory())))
		{
			ret = x.getName().compareTo(y.getName());
		} 
		else 
		{
			if (x.getDirectory())
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
