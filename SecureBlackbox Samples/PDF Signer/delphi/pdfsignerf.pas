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
unit pdfsignerf;

interface

uses
  SysUtils, Variants, Classes, Graphics, StrUtils,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxCertificateManager, SBxPDFSigner;

type
  TFormPdfsigner = class(TForm)
    Label10: TLabel;
    lbInputFile: TLabel;
    sbBrowseInputFile: TSpeedButton;
    Label1: TLabel;
    sbOutputFile: TSpeedButton;
    edInputFile: TEdit;
    edOutputFile: TEdit;
    GroupBox6: TGroupBox;
    Label3: TLabel;
    sbSignCertFile: TSpeedButton;
    Label5: TLabel;
    Label4: TLabel;
    edSigningCertificate: TEdit;
    edCertPassword: TEdit;
    cbLevel: TComboBox;
    cbVisible: TCheckBox;
    btnSign: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    cbHashAlgorithm: TComboBox;
    Label6: TLabel;
    cbRequestTimestamp: TCheckBox;
    editTSPServer: TEdit;
    Label7: TLabel;
    cbSignatureType: TComboBox;
    procedure sbBrowseInputFileClick(Sender: TObject);
    procedure sbOutputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure sbSignCertFileClick(Sender: TObject);
    procedure cbSignatureTypeChange(Sender: TObject);
  private
    { Private declarations }
    FSigner: TsbxPDFSigner;
  public
    { Public declarations }
  end;

var
  FormPdfsigner: TFormPdfsigner;

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

procedure TFormPdfsigner.btnSignClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
begin
  FSigner.InputFile := edInputFile.Text;
  FSigner.OutputFile := edOutputFile.Text;

  CertificateManager := TsbxCertificateManager.Create(nil);
  try
    try
      CertificateManager.ImportFromFile(edSigningCertificate.Text, edCertPassword.Text);

      FSigner.SigningCertificate := CertificateManager.Certificate;

      if ContainsText(FSigner.SigningCertificate.KeyAlgorithm, 'id-dsa') then
      begin
        MessageDlg('The certificate was found to contain a DSA key. The hash algorithm has been switched to SHA1.', mtInformation, [mbOk], 0);
        FSigner.NewSignature.HashAlgorithm := 'SHA1';
      end;
    except
      on E: Exception do
        MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
    end;
  finally
    FreeAndNil(CertificateManager);
  end;

  if cbSignatureType.ItemIndex = 0 then
    FSigner.NewSignature.SignatureType := pstLegacy
  else if cbSignatureType.ItemIndex = 2 then
    FSigner.NewSignature.SignatureType := pstDocumentTimestamp
  else
  begin
    FSigner.NewSignature.SignatureType := pstPAdES;
    case cbLevel.ItemIndex of
      0 : FSigner.NewSignature.Level := paslGeneric;
      1 : FSigner.NewSignature.Level := paslBaselineB;
      2 : FSigner.NewSignature.Level := paslBaselineT;
      3 : FSigner.NewSignature.Level := paslBaselineLT;
      4 : FSigner.NewSignature.Level := paslBaselineLTA;
      5 : FSigner.NewSignature.Level := paslBES;
      6 : FSigner.NewSignature.Level := paslEPES;
      7 : FSigner.NewSignature.Level := paslLTV;
    else
      FSigner.NewSignature.Level := paslBaselineB;
    end;
  end;

  FSigner.NewSignature.HashAlgorithm := cbHashAlgorithm.Text;
  FSigner.Widget.Invisible := not cbVisible.Checked;

  FSigner.IgnoreChainValidationErrors := true;
  FSigner.NewSignature.PolicyID := '1.2.3.4.5';

  if cbRequestTimestamp.Checked then
    FSigner.TimestampServer := editTSPServer.Text
  else
    FSigner.TimestampServer := '';

  try
    FSigner.Sign();

    MessageDlg('PDF file successfully signed', mtInformation, [mbOk], 0);
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormPdfsigner.cbSignatureTypeChange(Sender: TObject);
begin
  cbLevel.Enabled := cbSignatureType.ItemIndex = 1;
end;

procedure TFormPdfsigner.FormCreate(Sender: TObject);
begin
  FSigner := TsbxPDFSigner.Create(nil);
end;

procedure TFormPdfsigner.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSigner);
end;

procedure TFormPdfsigner.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edInputFile.Text;
  dlgOpen.Filter := 'PDF files (*.pdf)|*.pdf|All files (*.*)|*.*';
  if dlgOpen.Execute then
    edInputFile.Text := dlgOpen.FileName;
end;

procedure TFormPdfsigner.sbOutputFileClick(Sender: TObject);
begin
  dlgSave.InitialDir := ExtractFilePath(Application.ExeName);
  dlgSave.Filter := 'PDF files (*.pdf)|*.pdf|All files (*.*)|*.*';
  if dlgSave.Execute then
    edOutputFile.Text := dlgSave.FileName;
end;

procedure TFormPdfsigner.sbSignCertFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edSigningCertificate.Text;
  dlgOpen.Filter := 'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX';
  if dlgOpen.Execute then
    edSigningCertificate.Text := dlgOpen.FileName;
end;

end.







