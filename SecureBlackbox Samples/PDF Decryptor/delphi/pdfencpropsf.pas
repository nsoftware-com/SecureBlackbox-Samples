unit pdfencpropsf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
  SBxTypes, SBxCore, SBxCertificateManager, SBxPDFDecryptor;

type
  TFormpdfencprops = class(TForm)
    gbEncryptionProps: TGroupBox;
    lCertificate: TLabel;
    lCertPassword: TLabel;
    editPassword: TEdit;
    editCert: TEdit;
    btnBrowseCert: TButton;
    editCertPassword: TEdit;
    lProvideCertificate: TLabel;
    lProvidePassword: TLabel;
    lEncryptionAlgorithm: TLabel;
    lMetadataStatus: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    OpenDialogCert: TOpenDialog;
    lInfo: TLabel;
    procedure btnBrowseCertClick(Sender: TObject);
  public
    FCount : Integer;

    procedure SetPDFEncryptionProps(PDFDecryptor : TsbxPDFDecryptor);
    procedure GetPDFDecryptionInfo(PDFDecryptor : TsbxPDFDecryptor);
  end;

var
  FormPdfencprops: TFormpdfencprops;

implementation

{$R *.dfm}

{ TFormpdfencprops }

procedure TFormpdfencprops.SetPDFEncryptionProps(PDFDecryptor: TsbxPDFDecryptor);
var
  Alg : string;
begin
  Alg := PDFDecryptor.DocumentInfo.EncryptionAlgorithm;
  if Alg = 'RC4' then
  begin
    Alg := Alg + '/' + PDFDecryptor.Config('RC4KeyBits');
  end;

  lEncryptionAlgorithm.Caption := 'Encryption algorithm: ' + Alg;
  if PDFDecryptor.DocumentInfo.EncryptionType = petPassword then
  begin
    if FCount > 0 then
      MessageDlg('Invalid password', mtWarning, [mbOk], 0);

    if PDFDecryptor.DocumentInfo.MetadataEncrypted then
      lMetadataStatus.Caption := 'Metadata status: encrypted'
    else
      lMetadataStatus.Caption := 'Metadata status: not encrypted';

    lProvidePassword.Enabled := true;
    editPassword.Enabled := true;
    lProvideCertificate.Enabled := false;
    editCert.Enabled := false;
    editCertPassword.Enabled := false;
    btnBrowseCert.Enabled := false;
  end
  else
  begin
    lMetadataStatus.Caption := ''; // metadata status could be read only after decryption

    lProvidePassword.Enabled := false;
    editPassword.Enabled := false;
    lProvideCertificate.Enabled := true;
    editCert.Enabled := true;
    editCertPassword.Enabled := true;
    btnBrowseCert.Enabled := true;
  end;
end;

procedure TFormpdfencprops.btnBrowseCertClick(Sender: TObject);
begin
  OpenDialogCert.Filename := editCert.Text;
  OpenDialogCert.Filter := 'Certificates (*.pem, *.cer, *.crt, *.der, *.pfx, *.p12, *.pkcs12)|*.pem;*.cer;*.crt;*.der;*.pfx;*.p12;*.pkcs12|PEM-encoded certificates (*.pem)|*.pem|' +
    'DER-encoded certificates (*.cer, *.crt, *.der)|*.cer;*.crt;*.der|PKCS#12 encoded certificates (*.pfx, *.p12, *.pkcs12)|*.pfx;*.p12;*.pkcs12|All files (*.*)|*.*';
  if OpenDialogCert.Execute then
    editCert.Text := OpenDialogCert.Filename;
end;

procedure TFormpdfencprops.GetPDFDecryptionInfo(PDFDecryptor : TsbxPDFDecryptor);
var
  CertificateManager: TsbxCertificateManager;
begin
  if PDFDecryptor.DocumentInfo.EncryptionType = petPassword then
    PDFDecryptor.Password := editPassword.Text
  else
  begin
    CertificateManager := TsbxCertificateManager.Create(nil);
    try
      try
        CertificateManager.ImportFromFile(editCert.Text, editCertPassword.Text);

        PDFDecryptor.DecryptionCertificate := CertificateManager.Certificate;
      except
        on E: Exception do
          MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
      end;
    finally
      FreeAndNil(CertificateManager);
    end;
  end;

  Inc(FCount);
end;

end.