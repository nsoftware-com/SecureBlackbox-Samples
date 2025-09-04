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
unit jadessignerf;

interface

uses
  SysUtils, Variants, Classes, Graphics, StrUtils,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxCertificateManager, SBxJAdESSigner;

type
  TFormJadessigner = class(TForm)
    Label10: TLabel;
    lbPayload: TLabel;
    Label1: TLabel;
    edOutputFile: TEdit;
    GroupBox6: TGroupBox;
    Label3: TLabel;
    sbSignCertFile: TSpeedButton;
    Label5: TLabel;
    Label4: TLabel;
    edSigningCertificate: TEdit;
    edCertPassword: TEdit;
    cbLevel: TComboBox;
    btnSign: TButton;
    dlgOpen: TOpenDialog;
    cbCompactForm: TCheckBox;
    sbOutputFile: TSpeedButton;
    dlgSave: TSaveDialog;
    mmPayload: TMemo;
    cbFlattenedSignature: TCheckBox;
    cbDetached: TCheckBox;
    cbHashAlgorithm: TComboBox;
    Label6: TLabel;
    cbRequestTimestamp: TCheckBox;
    editTSPServer: TEdit;
    procedure sbOutputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure sbSignCertFileClick(Sender: TObject);
  private
    { Private declarations }
    FSigner: TsbxJAdESSigner;
  public
    { Public declarations }
  end;

var
  FormJadessigner: TFormJadessigner;

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

procedure TFormJadessigner.btnSignClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
begin
  FSigner.OutputFile := edOutputFile.Text;

  FSigner.DataString := Trim(mmPayload.Text);

  FSigner.Detached := cbDetached.Checked;
  FSigner.NewSignature.SignedDataType := jasdtPayload;

  FSigner.CompactForm := cbCompactForm.Checked;
  FSigner.FlattenedSignature := cbFlattenedSignature.Checked;

  CertificateManager := TsbxCertificateManager.Create(nil);
  try
    try
      CertificateManager.ImportFromFile(edSigningCertificate.Text, edCertPassword.Text);

      FSigner.SigningCertificate := CertificateManager.Certificate;
    except
      on E: Exception do
      begin
        MessageDlg('Failed to load certificate: ' + E.Message, mtError, [mbOk], 0);
        Exit;
      end;
    end;
  finally
    FreeAndNil(CertificateManager);
  end;

  case cbLevel.ItemIndex of
    0: FSigner.NewSignature.Level := jaslGeneric;
    1: FSigner.NewSignature.Level := jaslBaselineB;
    2: FSigner.NewSignature.Level := jaslBaselineT;
    3: FSigner.NewSignature.Level := jaslBaselineLT;
    4: FSigner.NewSignature.Level := jaslBaselineLTA;
  end;

  FSigner.NewSignature.HashAlgorithm := cbHashAlgorithm.Text;

  FSigner.NewSignature.PolicyID := '1.2.3.4.5';

  if cbRequestTimestamp.Checked then
  begin
    if (FSigner.NewSignature.Level = jaslBaselineB) or
       (FSigner.NewSignature.Level = jaslGeneric) then
      FSigner.NewSignature.Level := jaslBaselineT;

    FSigner.TimestampServer := editTSPServer.Text
  end
  else
    FSigner.TimestampServer := '';

  try
    FSigner.Sign();

    MessageDlg('JWS/JAdES signature successfully created', mtInformation, [mbOk], 0);
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormJadessigner.FormCreate(Sender: TObject);
begin
  FSigner := TsbxJAdESSigner.Create(nil);
end;

procedure TFormJadessigner.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSigner);
end;

procedure TFormJadessigner.sbOutputFileClick(Sender: TObject);
begin
  dlgSave.InitialDir := ExtractFilePath(Application.ExeName);
  dlgSave.DefaultExt := 'json';
  dlgSave.Filter := 'JSON files (*.json)|*.json|All files (*.*)|*.*';
  if dlgSave.Execute then
    edOutputFile.Text := dlgSave.FileName;
end;

procedure TFormJadessigner.sbSignCertFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edSigningCertificate.Text;
  dlgOpen.Filter := 'Certificates (*.pem, *.cer, *.crt, *.der, *.pfx, *.p12, *.pkcs12)|*.pem;*.cer;*.crt;*.der;*.pfx;*.p12;*.pkcs12|PEM-encoded certificates (*.pem)|*.pem|' +
    'DER-encoded certificates (*.cer, *.crt, *.der)|*.cer;*.crt;*.der|PKCS#12 encoded certificates (*.pfx, *.p12, *.pkcs12)|*.pfx;*.p12;*.pkcs12|All files (*.*)|*.*';
  if dlgOpen.Execute then
    edSigningCertificate.Text := dlgOpen.FileName;
end;

end.


