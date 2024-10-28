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
unit authenticodesignerf;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  sbxcore, sbxtypes, sbxcertificatemanager, sbxauthenticodesigner;

type
  TFormAuthenticodesigner = class(TForm)
    lblInfo: TLabel;
    Label2: TLabel;
    lblOutputFile: TLabel;
    edtInputFile: TEdit;
    btnInputBrowse: TButton;
    edtOutputFile: TEdit;
    btnOutputBrowse: TButton;
    grpSigningCertificate: TGroupBox;
    lblCertFilename: TLabel;
    btnCertFileBrowse: TButton;
    edtCertFile: TEdit;
    lblCertPassword: TLabel;
    edtCertPassword: TEdit;
    cbxCertShowPassword: TCheckBox;
    grpSignature: TGroupBox;
    lblDescription: TLabel;
    edtDescription: TEdit;
    lblUrl: TLabel;
    edtUrl: TEdit;
    lblHashAlgorithm: TLabel;
    cmbHashAlgorithm: TComboBox;
    lblStatement: TLabel;
    rbtIndividual: TRadioButton;
    rbtCommercial: TRadioButton;
    lblSigningTime: TLabel;
    edtSigningTime: TDateTimePicker;
    grpTimestamp: TGroupBox;
    rbtNoTimestamp: TRadioButton;
    rbtTrusted: TRadioButton;
    lblTrustedServer: TLabel;
    cmbTrustedServer: TComboBox;
    rbtLegacy: TRadioButton;
    lblLegacyServer: TLabel;
    cmbLegacyServer: TComboBox;
    btnSign: TButton;
    cbxRemove: TCheckBox;
    dlgOpenExe: TOpenDialog;
    dlgSaveExe: TSaveDialog;
    dlgOpenCert: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnInputBrowseClick(Sender: TObject);
    procedure btnOutputBrowseClick(Sender: TObject);
    procedure btnCertFileBrowseClick(Sender: TObject);
    procedure cbxCertShowPasswordClick(Sender: TObject);
    procedure rbtTrustedClick(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSigner: TsbxAuthenticodeSigner;
    function GetHashAlgorithm(): string;
  public
    { Public declarations }
  end;

var
  FormAuthenticodesigner: TFormAuthenticodesigner;

implementation

{$R *.dfm}

function GetDateTimeFormat(): string;
begin
  Result := {$if CompilerVersion >= 24.0}FormatSettings.{$ifend}ShortDateFormat + ' ' +
    {$if CompilerVersion >= 24.0}FormatSettings.{$ifend}ShortTimeFormat;
end;

procedure TFormAuthenticodesigner.btnCertFileBrowseClick(Sender: TObject);
begin
  if edtCertFile.Text <> '' then
  begin
    dlgOpenCert.InitialDir := ExtractFilePath(edtCertFile.Text);
    dlgOpenCert.FileName := ExtractFileName(edtCertFile.Text);
  end;

  if dlgOpenCert.Execute(Self.Handle) then
    edtCertFile.Text := dlgOpenCert.FileName;
end;

procedure TFormAuthenticodesigner.btnInputBrowseClick(Sender: TObject);
var
  Path, Filename: string;
begin
  if edtInputFile.Text <> '' then
    dlgOpenExe.InitialDir := ExtractFilePath(edtInputFile.Text);

  if dlgOpenExe.Execute(Self.Handle) then
  begin
    edtInputFile.Text := dlgOpenExe.FileName;
    Path := ExtractFilePath(dlgOpenExe.FileName);
    Filename := ExtractFileName(dlgOpenExe.FileName);
    edtOutputFile.Text := Path + 'Signed' + Filename;
  end;
end;

procedure TFormAuthenticodesigner.btnOutputBrowseClick(Sender: TObject);
begin
  if edtOutputFile.Text <> '' then
  begin
    dlgSaveExe.InitialDir := ExtractFilePath(edtOutputFile.Text);
    dlgSaveExe.FileName := ExtractFileName(edtOutputFile.Text);
  end;

  if dlgSaveExe.Execute(Self.Handle) then
    edtOutputFile.Text := dlgSaveExe.FileName;
end;

procedure TFormAuthenticodesigner.btnSignClick(Sender: TObject);
var
  Manager: TsbxCertificateManager;
begin
  // load a signing certificate

  Manager := TsbxCertificateManager.Create(nil);
  try
    try
      Manager.ImportFromFile(edtCertFile.Text, edtCertPassword.Text);
      FSigner.SigningCertificate := Manager.Certificate;
    except
      on E: Exception do
      begin
        MessageDlg('Failed to load a signing certificate.'#13#10 + E.Message, mtError, [mbOk], 0);
        Exit;
      end;
    end;
  finally
    FreeAndNil(Manager);
  end;

  // setup the signer

  FSigner.InputFile := edtInputFile.Text;
  FSigner.OutputFile := edtOutputFile.Text;
  FSigner.SignatureDescription := edtDescription.Text;
  FSigner.SignatureURL := edtUrl.Text;
  FSigner.HashAlgorithm := GetHashAlgorithm();
  if rbtIndividual.Checked then
    FSigner.StatementType := acsIndividual
  else
    FSigner.StatementType := acsCommercial;
  if edtSigningTime.Checked then
    FSigner.ClaimedSigningTime := DateTimeToStr(edtSigningTime.DateTime)
  else
    FSigner.ClaimedSigningTime := '';
  if rbtNoTimestamp.Checked then
    FSigner.TimestampServer := ''
  else
  if rbtTrusted.Checked then
  begin
    FSigner.TimestampType := actTrusted;
    FSigner.TimestampServer := cmbTrustedServer.Text;
  end
  else
  begin
    FSigner.TimestampType := actLegacy;
    FSigner.TimestampServer := cmbLegacyServer.Text;
  end;
  FSigner.RemoveExistingSignatures := cbxRemove.Checked;

  try
    FSigner.Sign();
    MessageDlg('The file has been successfully signed', mtInformation, [mbOk], 0);
  except
    on E: Exception do
      MessageDlg('Failed to sign the file.'#13#10 + E.Message, mtInformation, [mbOk], 0);
  end;
end;

procedure TFormAuthenticodesigner.cbxCertShowPasswordClick(Sender: TObject);
begin
  if cbxCertShowPassword.Checked then
    edtCertPassword.PasswordChar := #0
  else
    edtCertPassword.PasswordChar := '*';
end;

procedure TFormAuthenticodesigner.FormCreate(Sender: TObject);
var
  ST: TSystemTime;
begin
  FSigner := TsbxAuthenticodeSigner.Create(nil);

  edtSigningTime.Format := GetDateTimeFormat();

  // signing time has to be in UTC
  GetSystemTime(ST);
  edtSigningTime.DateTime := EncodeDate(ST.wYear, ST.wMonth, ST.wDay) + EncodeTime(ST.wHour, ST.wMinute, ST.wSecond, ST.wMilliseconds);
  edtSigningTime.Checked := False;

  cmbTrustedServer.ItemIndex := 0;
  cmbLegacyServer.ItemIndex := 0;
end;

procedure TFormAuthenticodesigner.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSigner);
end;

function TFormAuthenticodesigner.GetHashAlgorithm(): string;
const
  HashAlgorithms: array [0..9] of string = (
      'md5', 'sha1', 'sha224', 'sha256', 'sha384', 'sha512',
      'sha3_224', 'sha3_256', 'sha3_384', 'sha3_512'
    );
begin
  if (cmbHashAlgorithm.ItemIndex < 0) or (cmbHashAlgorithm.ItemIndex >= Length(HashAlgorithms)) then
    Result := ''
  else
    Result := HashAlgorithms[cmbHashAlgorithm.ItemIndex];
end;

procedure TFormAuthenticodesigner.rbtTrustedClick(Sender: TObject);
begin
  lblTrustedServer.Enabled := rbtTrusted.Checked;
  cmbTrustedServer.Enabled := rbtTrusted.Checked;
  lblLegacyServer.Enabled := rbtLegacy.Checked;
  cmbLegacyServer.Enabled := rbtLegacy.Checked;
end;

end.

