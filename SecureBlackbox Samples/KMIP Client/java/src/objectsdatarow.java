import secureblackbox.*;

public class objectsdatarow {

	public String UniqueIdentifier;
	public String ObjType;
	public String Algorithm;
	public int Length;
	public String Id;
	public KMIPObject Tag;

	public objectsdatarow(KMIPObject obj)
	{
		Tag = obj;

		UniqueIdentifier = obj.getUniqueIdentifier();

		switch (obj.getObjectType())
		{
			case KMIPObject.otCertificate: ObjType = "Certificate"; break;
			case KMIPObject.otSymmetricKey: ObjType = "Symmetric Key"; break;
			case KMIPObject.otPublicKey: ObjType = "Public Key"; break;
			case KMIPObject.otPrivateKey: ObjType = "Private Key"; break;
			default: ObjType = "Unknown"; break;
		}

		Algorithm = obj.getKeyAlgorithm();
		Length = obj.getKeyLength();
		Id = obj.getID();
	}
}
