unit xmlencpropsf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
  SBxTypes, SBxCore, SBxCertificateManager, SBxXMLDecryptor;

type
  TFormXmlencprops = class(TForm)
    gbEncryptionProps: TGroupBox;
    btnOK: TButton;
    OpenDialogCert: TOpenDialog;
    lInfo: TLabel;
    GroupBox1: TGroupBox;
    lbCertificate: TLabel;
    lbCertPassword: TLabel;
    lbPassphrase: TLabel;
    edPassphrase: TEdit;
    edCert: TEdit;
    btnBrowseCert: TButton;
    edCertPassword: TEdit;
    mmInfo: TMemo;
    procedure btnBrowseCertClick(Sender: TObject);
  protected
    function GetKey(const Algorithm: string): TBytes;
  public
    FCount : Integer;

    procedure SetXMLEncryptionProps(XMLDecryptor : TsbxXMLDecryptor);
    procedure GetXMLDecryptionInfo(XMLDecryptor : TsbxXMLDecryptor);
  end;

var
  FormXmlencprops: TFormXmlencprops;

implementation

{$R *.dfm}

{ TFormXmlencprops }

procedure TFormXmlencprops.SetXMLEncryptionProps(XMLDecryptor: TsbxXMLDecryptor);
var
  s, t : string;
  CertRequired : Boolean;
begin
  s := '';
  t := '';
  if XMLDecryptor.UseGCM then
    t := '-GCM';

  s := 'Encryption method: ' + XMLDecryptor.EncryptionMethod + t + #13#10;
  if XMLDecryptor.EncryptedDataType = cxedtElement then
    t := 'Element'
  else
  if XMLDecryptor.EncryptedDataType = cxedtContent then
    t := 'Content'
  else
    t := 'External';

  s := s + 'Encrypted data type: ' + t + #13#10;
  CertRequired := false;
  if XMLDecryptor.EncryptKey then
  begin
    s := s + 'EncryptKey: true'#13#10;
    if XMLDecryptor.KeyEncryptionType = cxetKeyTransport then
    begin
      s := s + 'Key encryption type: transport'#13#10;
      if XMLDecryptor.KeyTransportMethod = cxktRSA15 then
        t := 'RSA v1.5'
      else
        t := 'RSA-OAEP';

      s := s + 'Key transport method: ' + t + #13#10;
      CertRequired := true;
    end
    else
    begin
      s := s + 'Key encryption type: wrap'#13#10;
      s := s + 'Key wrap method: ' + XMLDecryptor.KeyWrapMethod + #13#10;
    end;
  end
  else
    s := s + 'EncryptKey: false'#13#10;

  t := XMLDecryptor.Config('KeyName');
  if Length(t) > 0 then
    s := s + 'Key name: ' + t + #13#10;

  t := XMLDecryptor.Config('MimeType');
  if Length(t) > 0 then
    s := s + 'Mime type: ' + t + #13#10;

  mmInfo.Text := s;

  edPassphrase.Enabled := not CertRequired;
  lbPassphrase.Enabled := not CertRequired;
  edCert.Enabled := CertRequired;
  lbCertificate.Enabled := CertRequired;
  edCertPassword.Enabled := CertRequired;
  lbCertPassword.Enabled := CertRequired;
  btnBrowseCert.Enabled := CertRequired;

  if CertRequired then
  begin
    if FCount > 0 then
      MessageDlg('Invalid certificate', mtWarning, [mbOk], 0);
  end
  else
  begin
    if FCount > 0 then
      MessageDlg('Invalid password', mtWarning, [mbOk], 0);
  end;
end;

procedure TFormXmlencprops.btnBrowseCertClick(Sender: TObject);
begin
  OpenDialogCert.Filename := edCert.Text;
  OpenDialogCert.Filter := 'Certificates (*.pem, *.cer, *.crt, *.der, *.pfx, *.p12, *.pkcs12)|*.pem;*.cer;*.crt;*.der;*.pfx;*.p12;*.pkcs12|PEM-encoded certificates (*.pem)|*.pem|' +
    'DER-encoded certificates (*.cer, *.crt, *.der)|*.cer;*.crt;*.der|PKCS#12 encoded certificates (*.pfx, *.p12, *.pkcs12)|*.pfx;*.p12;*.pkcs12|All files (*.*)|*.*';
  if OpenDialogCert.Execute then
    edCert.Text := OpenDialogCert.Filename;
end;

function TFormXmlencprops.GetKey(const Algorithm: string): TBytes;
var
  i, Len : Integer;
  s : string;
begin
  if Algorithm = 'AES128' then
    Len := 16
  else if Algorithm = 'AES192' then
    Len := 24
  else if Algorithm = 'AES256' then
    Len := 32
  else if Algorithm = 'Camellia128' then
    Len := 16
  else if Algorithm = 'Camellia192' then
    Len := 24
  else if Algorithm = 'Camellia256' then
    Len := 32
  else if Algorithm = 'DES' then
    Len := 8
  else if Algorithm = '3DES' then
    Len := 24
  else if Algorithm = 'RC4' then
    Len := 16
  else if Algorithm = 'SEED' then
    Len := 16
  else
    Len := 0;

  // simple key derivation function from a Passphrase
  // TODO: replace with SHA256 hash or KDF
  s := edPassphrase.Text;
  while Length(s) < Len do
    s := s + '/' + s;

  SetLength(Result, Len);
  for i := 0 to Len - 1 do
    Result[i] := Byte(s[i + 1]);
end;

procedure TFormXmlencprops.GetXMLDecryptionInfo(XMLDecryptor : TsbxXMLDecryptor);
var
  CertificateManager: TsbxCertificateManager;
begin
  if XMLDecryptor.EncryptKey then
  begin
    if XMLDecryptor.KeyEncryptionType = cxetKeyTransport then
    begin
      CertificateManager := TsbxCertificateManager.Create(nil);
      try
        try
          CertificateManager.ImportFromFile(edCert.Text, edCertPassword.Text);

          XMLDecryptor.KeyDecryptionCertificate := CertificateManager.Certificate;
        except
          on E: Exception do
            MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
        end;
      finally
        FreeAndNil(CertificateManager);
      end;
    end
    else
    begin
      XMLDecryptor.KeyDecryptionKey := GetKey(XMLDecryptor.KeyWrapMethod);
    end;
  end
  else
  begin
    XMLDecryptor.DecryptionKey := GetKey(XMLDecryptor.EncryptionMethod);
  end;

  Inc(FCount);
end;

end.
