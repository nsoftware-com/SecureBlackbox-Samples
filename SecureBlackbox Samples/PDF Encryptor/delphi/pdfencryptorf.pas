(*
 * SecureBlackbox 2022 Delphi Edition - Sample Project
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
unit pdfencryptorf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  SBxTypes, SBxCore, SBxCertificateManager, SBxPDFEncryptor;

type
  TFormPdfencryptor = class(TForm)
    lSourceFile: TLabel;
    editSource: TEdit;
    lDestFile: TLabel;
    editDest: TEdit;
    btnBrowseSource: TButton;
    btnBrowseDest: TButton;
    gbEncryptionProps: TGroupBox;
    rbPasswordEncryption: TRadioButton;
    rbPublicKeyEncryption: TRadioButton;
    editPassword: TEdit;
    lCertificate: TLabel;
    editCert: TEdit;
    btnBrowseCert: TButton;
    lCertPassword: TLabel;
    editCertPassword: TEdit;
    cbAlgorithm: TComboBox;
    lEncryptionAlgorithm: TLabel;
    cbEncryptMetadata: TCheckBox;
    btnEncrypt: TButton;
    btnCancel: TButton;
    OpenDialogPDF: TOpenDialog;
    SaveDialogPDF: TSaveDialog;
    OpenDialogCert: TOpenDialog;
    lDemoInfo: TLabel;
    lPassword: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseSourceClick(Sender: TObject);
    procedure btnBrowseDestClick(Sender: TObject);
    procedure btnBrowseCertClick(Sender: TObject);
    procedure rbPasswordEncryptionClick(Sender: TObject);
    procedure rbPublicKeyEncryptionClick(Sender: TObject);
    procedure btnEncryptClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  FormPdfencryptor: TFormPdfencryptor;

implementation

{$R *.DFM}

procedure TFormPdfencryptor.FormCreate(Sender: TObject);
begin
  cbAlgorithm.ItemIndex := 0;
end;

procedure TFormPdfencryptor.btnBrowseSourceClick(Sender: TObject);
begin
  OpenDialogPDF.Filename := editSource.Text;
  OpenDialogPDF.Filter := 'PDF files (*.pdf)|*.pdf|All files (*.*)|*.*';
  if OpenDialogPDF.Execute then
    editSource.Text := OpenDialogPDF.Filename;
end;

procedure TFormPdfencryptor.btnBrowseDestClick(Sender: TObject);
begin
  SaveDialogPDF.FileName := editDest.Text;
  SaveDialogPDF.Filter := 'PDF files (*.pdf)|*.pdf|All files (*.*)|*.*';
  if SaveDialogPDF.Execute then
    editDest.Text := SaveDialogPDF.FileName;
end;

procedure TFormPdfencryptor.btnBrowseCertClick(Sender: TObject);
begin
  OpenDialogCert.Filename := editCert.Text;
  OpenDialogCert.Filter := 'Raw X.509 certificate (*.cer, *.csr, *.crt)|*.CER;*.CSR;*.CRT|PEM-encoded X.509 certificate (*.pem)|*.PEM|PKCS#12 certificate (*.pfx, *.p12)|*.PFX; *.P12|All files (*.*)|*.*';
  if OpenDialogCert.Execute then
    editCert.Text := OpenDialogCert.Filename;
end;

procedure TFormPdfencryptor.rbPasswordEncryptionClick(Sender: TObject);
begin
  editPassword.Enabled := true;
  lPassword.Enabled := true;
  editCert.Enabled := false;
  editCertPassword.Enabled := false;
  lCertificate.Enabled := false;
  lCertPassword.Enabled := false;
  btnBrowseCert.Enabled := false;

  if cbAlgorithm.Items.Count = 4 then
    cbAlgorithm.Items.Add('AES/256 bits (Acrobat X)');
end;

procedure TFormPdfencryptor.rbPublicKeyEncryptionClick(Sender: TObject);
begin
  editPassword.Enabled := false;
  lPassword.Enabled := false;
  editCert.Enabled := true;
  editCertPassword.Enabled := true;
  lCertificate.Enabled := true;
  lCertPassword.Enabled := true;
  btnBrowseCert.Enabled := true;

  if cbAlgorithm.Items.Count = 5 then
    cbAlgorithm.Items.Delete(4);
end;

procedure TFormPdfencryptor.btnEncryptClick(Sender: TObject);
var
  PDFEncryptor : TsbxPDFEncryptor;
  CertificateManager: TsbxCertificateManager;
begin
  PDFEncryptor := TsbxPDFEncryptor.Create(nil);
  try
    try
      PDFEncryptor.InputFile := editSource.Text;
      PDFEncryptor.OutputFile := editDest.Text;

      if rbPasswordEncryption.Checked then
      begin
        PDFEncryptor.UserPassword := editPassword.Text;
        PDFEncryptor.OwnerPassword := editPassword.Text;
      end
      else
      begin

        CertificateManager := TsbxCertificateManager.Create(nil);
        try
          try
            CertificateManager.ImportFromFile(editCert.Text, editCertPassword.Text);

            PDFEncryptor.EncryptionCertificate := CertificateManager.Certificate;
          except
            on E: Exception do
              MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
          end;
        finally
          FreeAndNil(CertificateManager);
        end;
      end;

      PDFEncryptor.EncryptMetadata := cbEncryptMetadata.Checked;

      if cbAlgorithm.ItemIndex <= 1 then
      begin
        PDFEncryptor.EncryptionAlgorithm := 'RC4';
        if cbAlgorithm.ItemIndex = 1 then
          PDFEncryptor.Config('RC4KeyBits=40')
        else
          PDFEncryptor.Config('RC4KeyBits=128');
      end
      else
      if cbAlgorithm.ItemIndex = 2 then
      begin
        PDFEncryptor.EncryptionAlgorithm := 'AES128';
      end
      else
      if cbAlgorithm.ItemIndex > 3 then
      begin
        PDFEncryptor.EncryptionAlgorithm := 'AES256';
        if rbPasswordEncryption.Checked then
        begin
          if cbAlgorithm.ItemIndex = 3 then
            PDFEncryptor.Config('HardenedKeyGeneration=false')
          else
            PDFEncryptor.Config('HardenedKeyGeneration=true');
        end;
      end;

      PDFEncryptor.Encrypt;

      MessageDlg('PDF file successfully encrypted', mtInformation, [mbOk], 0);
    except
      on E: Exception do
        MessageDlg(e.Message, mtError, [mbOk], 0);
    end;
  finally
    FreeAndNil(PDFEncryptor);
  end;

  Close();
end;

procedure TFormPdfencryptor.btnCancelClick(Sender: TObject);
begin
  Close();
end;

end.


