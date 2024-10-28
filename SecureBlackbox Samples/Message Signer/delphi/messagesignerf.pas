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
unit messagesignerf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxCertificateManager, SBxMessageSigner;

type
  TFormMessagesigner = class(TForm)
    dlgOpenFile: TOpenDialog;
    dlgSaveFile: TSaveDialog;
    btnSign: TButton;
    DlgOpenCert: TOpenDialog;
    Label10: TLabel;
    GroupBox6: TGroupBox;
    Label2: TLabel;
    sbSignCertFile: TSpeedButton;
    Label3: TLabel;
    edSigningCertificate: TEdit;
    edCertPassword: TEdit;
    lbSignatureType: TLabel;
    lbHashAlgorithm: TLabel;
    cmbSignatureType: TComboBox;
    cmbHashAlgorithm: TComboBox;
    lbInputFile: TLabel;
    sbBrowseInputFile: TSpeedButton;
    Label1: TLabel;
    sbOutputFile: TSpeedButton;
    edInputFile: TEdit;
    edOutputFile: TEdit;
    procedure sbBrowseInputFileClick(Sender: TObject);
    procedure sbOutputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure sbSignCertFileClick(Sender: TObject);
  private
    { Private declarations }
    FMessageSigner: TsbxMessageSigner;
  public
    { Public declarations }
  end;

var
  FormMessagesigner: TFormMessagesigner;

implementation

{$R *.dfm}

function BinaryToString(const Buffer : TBytes): string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to Length(Buffer) - 1 do
    Result := Result + IntToHex(Buffer[i], 1);
end;

procedure TFormMessagesigner.btnSignClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
begin
  FMessageSigner.InputFile := edInputFile.Text;
  FMessageSigner.OutputFile := edOutputFile.Text;

  case (cmbSignatureType.ItemIndex) of
    1:
      FMessageSigner.SignatureType := stPKCS7Detached;
    2:
      FMessageSigner.SignatureType := stPKCS7Enveloping;
    else
      FMessageSigner.SignatureType := stPKCS1Detached;
  end;

  FMessageSigner.HashAlgorithm := cmbHashAlgorithm.Text;

  CertificateManager := TsbxCertificateManager.Create(nil);
  try
    try
      CertificateManager.ImportFromFile(edSigningCertificate.Text, edCertPassword.Text);

      FMessageSigner.SigningCertificate := CertificateManager.Certificate;
    except
      on E: Exception do
        MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
    end;
  finally
    FreeAndNil(CertificateManager);
  end;

  try
    FMessageSigner.Sign();

    MessageDlg('The file successfully signed', mtInformation, [mbOk], 0);
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormMessagesigner.FormCreate(Sender: TObject);
begin
  FMessageSigner := TsbxMessageSigner.Create(nil);

  cmbSignatureType.ItemIndex := 1;
  cmbHashAlgorithm.ItemIndex := 2;
end;

procedure TFormMessagesigner.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMessageSigner);
end;

procedure TFormMessagesigner.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpenFile.FileName := edInputFile.Text;
  if dlgOpenFile.Execute then
    edInputFile.Text := dlgOpenFile.FileName;
end;

procedure TFormMessagesigner.sbOutputFileClick(Sender: TObject);
begin
  if dlgSaveFile.Execute then
    edOutputFile.Text := dlgSaveFile.FileName;
end;

procedure TFormMessagesigner.sbSignCertFileClick(Sender: TObject);
begin
  DlgOpenCert.FileName := edSigningCertificate.Text;
  if DlgOpenCert.Execute then
    edSigningCertificate.Text := DlgOpenCert.FileName;
end;

end.





