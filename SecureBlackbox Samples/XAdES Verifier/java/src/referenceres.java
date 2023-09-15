public class referenceres {
    public String Id;
    public String Uri;
    public String RefType;
    public Boolean DigestValid;

    public referenceres(String id, String uri, String refType, Boolean digestValid){
        Id = id;
        Uri = uri;
        RefType = refType;
        DigestValid = digestValid;
    }
}