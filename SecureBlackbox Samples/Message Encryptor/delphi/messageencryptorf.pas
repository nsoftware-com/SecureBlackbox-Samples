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
unit messageencryptorf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxCertificateManager, SBxMessageEncryptor;

type
  TFormMessageencryptor = class(TForm)
    lbInputFile: TLabel;
    edInputFile: TEdit;
    sbBrowseInputFile: TSpeedButton;
    dlgOpenFile: TOpenDialog;
    Label1: TLabel;
    edOutputFile: TEdit;
    sbOutputFile: TSpeedButton;
    dlgSaveFile: TSaveDialog;
    btnEncrypt: TButton;
    DlgOpenCert: TOpenDialog;
    Label10: TLabel;
    GroupBox1: TGroupBox;
    lbSymmetricAlgorithm: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    sbCertFile: TSpeedButton;
    cmbEncryptionAlgorithm: TComboBox;
    edCertificate: TEdit;
    edCertPassword: TEdit;
    procedure sbBrowseInputFileClick(Sender: TObject);
    procedure sbOutputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnEncryptClick(Sender: TObject);
    procedure sbCertFileClick(Sender: TObject);
  private
    { Private declarations }
    FMessageEncryptor: TsbxMessageEncryptor;
  public
    { Public declarations }
  end;

var
  FormMessageencryptor: TFormMessageencryptor;

implementation

{$R *.dfm}

procedure TFormMessageencryptor.btnEncryptClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
begin
  FMessageEncryptor.InputFile := edInputFile.Text;
  FMessageEncryptor.OutputFile := edOutputFile.Text;

  FMessageEncryptor.EncryptionAlgorithm := cmbEncryptionAlgorithm.Text;


  CertificateManager := TsbxCertificateManager.Create(nil);
  try
    try
      CertificateManager.ImportFromFile(edCertificate.Text, edCertPassword.Text);

      FMessageEncryptor.EncryptionCertificates.Add(CertificateManager.Certificate);
    except
      on E: Exception do
        MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
    end;
  finally
    FreeAndNil(CertificateManager);
  end;

  try
    FMessageEncryptor.Encrypt();

    MessageDlg('The file successfully encrypted', mtInformation, [mbOk], 0);
  except
    on E: Exception do
    begin
      MessageDlg(e.message, mtError, [mbOk], 0);
    end;
  end;
end;

procedure TFormMessageencryptor.FormCreate(Sender: TObject);
begin
  FMessageEncryptor := TsbxMessageEncryptor.Create(nil);

  cmbEncryptionAlgorithm.ItemIndex := 3;
end;

procedure TFormMessageencryptor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMessageEncryptor);
end;

procedure TFormMessageencryptor.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpenFile.FileName := edInputFile.Text;
  if dlgOpenFile.Execute then
    edInputFile.Text := dlgOpenFile.FileName;
end;

procedure TFormMessageencryptor.sbOutputFileClick(Sender: TObject);
begin
  if dlgSaveFile.Execute then
    edOutputFile.Text := dlgSaveFile.FileName;
end;

procedure TFormMessageencryptor.sbCertFileClick(Sender: TObject);
begin
  DlgOpenCert.FileName := edCertificate.Text;
  if DlgOpenCert.Execute then
    edCertificate.Text := DlgOpenCert.FileName;
end;

end.
