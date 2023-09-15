import secureblackbox.*;

public class filesdatarow {

	public String Name;
	public String Size;
	public String FileDate;
	public String FileType;
	public FTPListEntry Tag;
	
	public filesdatarow(FTPListEntry info) {
		Tag = info;
		
		Name = info.getName();
		Size = info.getSize() + "";
		FileDate = info.getFileDate();
		FileType = fileTypeName(info.getFileType());
	}
	
	private String fileTypeName(int FileType)
	{
		switch (FileType)
		{
			case FTPListEntry.cfetDirectory:
				return "Folder";
			default:
				return "File";
		}
	}
}
