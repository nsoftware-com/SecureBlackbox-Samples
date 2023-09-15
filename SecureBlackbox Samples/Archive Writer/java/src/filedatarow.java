
public class filedatarow {

	public String FileName;
	public long CompressSize;
	public long Size;
	public String Security;
	public int Action;
	public Object Tag;

	public filedatarow(String fileName, long csize, long size, String security, int act, Object tag) {
		FileName = fileName;
		CompressSize = csize;
		Size = size;
		Security = security;
		Action = act;
		Tag = tag;
	}

	public filedatarow(String fileName, Object tag) {
		FileName = fileName;
		Action = -1;
		Tag = tag;
	}
}
