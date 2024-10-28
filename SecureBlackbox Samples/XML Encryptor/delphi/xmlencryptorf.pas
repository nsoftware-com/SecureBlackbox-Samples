(*
 * SecureBlackbox 2024 Delphi Edition - Sample Project
 *
 * This sample project demonstrates the usage of SecureBlackbox in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/secureblackbox
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 *)
unit xmlencryptorf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons,
  SBxTypes, SBxCore, SBxCertificateManager, SBxXMLEncryptor;

type
  TFormXmlencryptor = class(TForm)
    lSourceFile: TLabel;
    edSource: TEdit;
    lDestFile: TLabel;
    edDest: TEdit;
    btnBrowseSource: TButton;
    btnBrowseDest: TButton;
    gbEncryptionProps: TGroupBox;
    edPassphrase: TEdit;
    lbCertificate: TLabel;
    edCert: TEdit;
    btnBrowseCert: TButton;
    lbCertPassword: TLabel;
    edCertPassword: TEdit;
    btnEncrypt: TButton;
    btnCancel: TButton;
    OpenDialogXML: TOpenDialog;
    SaveDialogXML: TSaveDialog;
    OpenDialogCert: TOpenDialog;
    lDemoInfo: TLabel;
    lbPassphrase: TLabel;
    gbGeneralEnc: TGroupBox;
    lbEncryptionMethod: TLabel;
    lbEncryptedDataType: TLabel;
    lbMimeType: TLabel;
    lbExternalFile: TLabel;
    sbExternalFile: TSpeedButton;
    cmbEncryptionMethod: TComboBox;
    cmbEncryptedDataType: TComboBox;
    edMimeType: TEdit;
    edExternalFile: TEdit;
    gbKEK: TGroupBox;
    lbKeyTransport: TLabel;
    lbKeyWrap: TLabel;
    rgKEK: TRadioGroup;
    cmbKeyTransport: TComboBox;
    cmbKeyWrap: TComboBox;
    OpenDialogExternal: TOpenDialog;
    cbEncryptKey: TCheckBox;
    edXMLNode: TEdit;
    lbXMLNode: TLabel;
    procedure btnBrowseSourceClick(Sender: TObject);
    procedure btnBrowseDestClick(Sender: TObject);
    procedure btnBrowseCertClick(Sender: TObject);
    procedure btnEncryptClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure cbEncryptKeyClick(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure rgKEKClick(Sender: TObject);
    procedure cmbEncryptedDataTypeChange(Sender: TObject);
    procedure sbExternalFileClick(Sender: TObject);
  protected
    procedure UpdateSettings;
    function GetKey(const Algorithm : string) : TBytes;
    function GetFileContents(const FileName : string) : TBytes;
  public
    { Public declarations }
  end;

var
  FormXmlencryptor: TFormXmlencryptor;

implementation

{$R *.DFM}

procedure TFormXmlencryptor.btnBrowseSourceClick(Sender: TObject);
begin
  OpenDialogXML.Filename := edSource.Text;
  OpenDialogXML.Filter := 'XML files (*.xml)|*.xml|All files (*.*)|*.*';
  if OpenDialogXML.Execute then
    edSource.Text := OpenDialogXML.Filename;
end;

procedure TFormXmlencryptor.btnBrowseDestClick(Sender: TObject);
begin
  SaveDialogXML.FileName := edDest.Text;
  SaveDialogXML.Filter := 'XML files (*.xml)|*.xml|All files (*.*)|*.*';
  if SaveDialogXML.Execute then
    edDest.Text := SaveDialogXML.FileName;
end;

procedure TFormXmlencryptor.btnBrowseCertClick(Sender: TObject);
begin
  OpenDialogCert.Filename := edCert.Text;
  OpenDialogCert.Filter := 'Raw X.509 certificate (*.cer, *.csr, *.crt)|*.cer;*.csr;*.crt|PEM-encoded X.509 certificate (*.pem)|*.pem|PKCS#12 certificate (*.pfx, *.p12)|*.pfx; *.p12|All files (*.*)|*.*';
  if OpenDialogCert.Execute then
    edCert.Text := OpenDialogCert.Filename;
end;

procedure TFormXmlencryptor.UpdateSettings;
begin
  rgKEK.Enabled := cbEncryptKey.Checked;
  cmbKeyTransport.Enabled := cbEncryptKey.Checked;
  lbKeyTransport.Enabled := cmbKeyTransport.Enabled;
  cmbKeyWrap.Enabled := cbEncryptKey.Checked;
  lbKeyWrap.Enabled := cmbKeyWrap.Enabled;
  cmbKeyTransport.Enabled := cbEncryptKey.Checked and (rgKEK.ItemIndex = 0);
  lbKeyTransport.Enabled := cmbKeyTransport.Enabled;
  cmbKeyWrap.Enabled := cbEncryptKey.Checked and (rgKEK.ItemIndex = 1);
  lbKeyWrap.Enabled := cmbKeyWrap.Enabled;
  edPassphrase.Enabled := cmbKeyWrap.Enabled or not cbEncryptKey.Checked;
  lbPassphrase.Enabled := edPassphrase.Enabled;
  edCert.Enabled := cmbKeyTransport.Enabled;
  lbCertificate.Enabled := edCert.Enabled;
  edCertPassword.Enabled := edCert.Enabled;
  lbCertPassword.Enabled := edCert.Enabled;
  btnBrowseCert.Enabled := edCert.Enabled;
  edMimeType.Enabled := (cmbEncryptedDataType.ItemIndex = 2);
  lbMimeType.Enabled := edMimeType.Enabled;
  edExternalFile.Enabled := (cmbEncryptedDataType.ItemIndex = 2);
  lbExternalFile.Enabled := edExternalFile.Enabled;
  sbExternalFile.Enabled := edExternalFile.Enabled;
end;

procedure TFormXmlencryptor.btnEncryptClick(Sender: TObject);
var
  XMLEncryptor : TsbxXMLEncryptor;
  CertificateManager: TsbxCertificateManager;
  s : string;
begin
  XMLEncryptor := TsbxXMLEncryptor.Create(nil);
  try
    try
      XMLEncryptor.InputFile := edSource.Text;
      XMLEncryptor.OutputFile := edDest.Text;

      XMLEncryptor.EncryptKey := cbEncryptKey.Checked;
      case cmbEncryptedDataType.ItemIndex of
        1 : XMLEncryptor.EncryptedDataType := cxedtContent;
        2 : XMLEncryptor.EncryptedDataType := cxedtExternal;
      else
        XMLEncryptor.EncryptedDataType := cxedtElement;
      end;

      s := cmbEncryptionMethod.Text;
      if Pos('-GCM', s) > 0 then
      begin
        s := Copy(s, 1, Length(s) - 4);
        XMLEncryptor.UseGCM := true;
      end;

      XMLEncryptor.EncryptionMethod := s;

      if XMLEncryptor.EncryptedDataType = cxedtExternal then
      begin
        if Length(edMimeType.Text) > 0 then
          XMLEncryptor.Config('MimeType=' + edMimeType.Text);

        XMLEncryptor.ExternalData := GetFileContents(edExternalFile.Text);
      end;

      XMLEncryptor.XMLNode := edXMLNode.Text;

      if XMLEncryptor.EncryptKey then
      begin
        if rgKEK.ItemIndex = 0 then
        begin
          XMLEncryptor.KeyEncryptionType := cxetKeyTransport;

          if cmbKeyTransport.ItemIndex = 0 then
            XMLEncryptor.KeyTransportMethod := cxktRSA15
          else
            XMLEncryptor.KeyTransportMethod := cxktRSAOAEP;

          CertificateManager := TsbxCertificateManager.Create(nil);
          try
            try
              CertificateManager.ImportFromFile(edCert.Text, edCertPassword.Text);

              XMLEncryptor.KeyEncryptionCertificate := CertificateManager.Certificate;
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
          XMLEncryptor.KeyEncryptionType := cxetKeyWrap;

          XMLEncryptor.KeyWrapMethod := cmbKeyWrap.Text;
          XMLEncryptor.KeyEncryptionKey := GetKey(XMLEncryptor.KeyWrapMethod);
        end;
      end
      else
      begin
        XMLEncryptor.EncryptionKey := GetKey(XMLEncryptor.EncryptionMethod);
      end;

      XMLEncryptor.Encrypt;

      MessageDlg('XML file successfully encrypted', mtInformation, [mbOk], 0);
      Close();
    except
      on E: Exception do
        MessageDlg(e.Message, mtError, [mbOk], 0);
    end;
  finally
    FreeAndNil(XMLEncryptor);
  end;
end;

procedure TFormXmlencryptor.cbEncryptKeyClick(Sender: TObject);
begin
  UpdateSettings;
end;

procedure TFormXmlencryptor.cmbEncryptedDataTypeChange(Sender: TObject);
begin
  UpdateSettings;
end;

procedure TFormXmlencryptor.FormCreate(Sender: TObject);
begin
  cmbEncryptionMethod.ItemIndex := 1;
  cmbEncryptedDataType.ItemIndex := 0;
  cmbKeyTransport.ItemIndex := 0;
  cmbKeyWrap.ItemIndex := 0;
  UpdateSettings;
end;

function TFormXmlencryptor.GetKey(const Algorithm: string): TBytes;
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

procedure TFormXmlencryptor.rgKEKClick(Sender: TObject);
begin
  UpdateSettings;
end;

procedure TFormXmlencryptor.sbExternalFileClick(Sender: TObject);
begin
  OpenDialogExternal.FileName := edExternalFile.Text;
  if OpenDialogExternal.Execute then
    edExternalFile.Text := OpenDialogExternal.FileName;
end;

function TFormXmlencryptor.GetFileContents(const FileName : string) : TBytes;
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, F.Size);
    F.Read(Result[0], Length(Result));
  finally
    FreeAndNil(F);
  end;
end;

procedure TFormXmlencryptor.btnCancelClick(Sender: TObject);
begin
  Close();
end;

end.


