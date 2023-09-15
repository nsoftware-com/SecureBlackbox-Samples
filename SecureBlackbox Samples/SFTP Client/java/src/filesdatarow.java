import secureblackbox.*;

public class filesdatarow {

	public String Name;
	public String Size;
	public String Modif;
	public String Owner;
	public String Rights;
	public SFTPListEntry Tag;

	public filesdatarow(SFTPListEntry info) {
		Tag = info;

		Name = info.getName();
		if (!info.getDirectory())
			Size = info.getSize() + "";

		Modif = info.getMTime().toString();
		Owner = info.getOwner();
		Rights = formatRights(info);
	}

	private String formatRights(SFTPListEntry entry)
	{
		String res = "";
		if (entry.getDirectory())
		{
			res = res + "d";
		}
		if (entry.getUserRead())
		{
			res = res + "r";
		}
		else
		{
			res = res + "-";
		}
		if (entry.getUserWrite())
		{
			res = res + "w";
		}
		else
		{
			res = res + "-";
		}
		if (entry.getUserExecute())
		{
			res = res + "x";
		}
		else
		{
			res = res + "-";
		}
		if (entry.getGroupRead())
		{
			res = res + 'r';
		}
		else
		{
			res = res + '-';
		}
		if (entry.getGroupWrite())
		{
			res = res + 'w';
		}
		else
		{
			res = res + '-';
		}
		if (entry.getGroupExecute())
		{
			res = res + 'x';
		}
		else
		{
			res = res + '-';
		}
		if (entry.getOtherRead())
		{
			res = res + 'r';
		}
		else
		{
			res = res + '-';
		}
		if (entry.getOtherWrite())
		{
			res = res + 'w';
		}
		else
		{
			res = res + '-';
		}
		if (entry.getOtherExecute())
		{
			res = res + 'x';
		}
		else
		{
			res = res + '-';
		}
		return res;
	}
}
