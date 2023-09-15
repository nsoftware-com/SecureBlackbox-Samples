
public class filedatarow {

	public String FileName;
	public long CompressSize;
	public long Size;
	public String Security;
	public Object Tag;

	public filedatarow(String fileName, long csize, long size, String security, Object tag) {
		FileName = fileName;
		CompressSize = csize;
		Size = size;
		Security = security;
		Tag = tag;
	}

	public filedatarow(String fileName, Object tag) {
		FileName = fileName;
		Tag = tag;
	}
}
