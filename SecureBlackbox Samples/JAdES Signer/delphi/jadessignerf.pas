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
    GroupBox5: TGroupBox;
    lvSigningCertificates: TListView;
    btnRemoveCert: TButton;
    btnAddCert: TButton;
    cbLevel: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    lvKnownCertificates: TListView;
    btnRemoveKnown: TButton;
    btnAddKnown: TButton;
    GroupBox4: TGroupBox;
    lvTrustedCertificates: TListView;
    btnRemoveTrusted: TButton;
    btnAddTrusted: TButton;
    cbIncludeLocalRevInfo: TCheckBox;
    btnSign: TButton;
    dlgOpen: TOpenDialog;
    GroupBox3: TGroupBox;
    cbIgnoreChainValidationErrors: TCheckBox;
    cbForceCompleteChainValidation: TCheckBox;
    editTSPServer: TEdit;
    cbRequestTimestamp: TCheckBox;
    GroupBox7: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    Label11: TLabel;
    cbHashAlgorithm: TComboBox;
    edIdentifier: TEdit;
    edHashValue: TEdit;
    cbCompactForm: TCheckBox;
    sbOutputFile: TSpeedButton;
    dlgSave: TSaveDialog;
    mmPayload: TMemo;
    cbFlattenedSignature: TCheckBox;
    cbDetached: TCheckBox;
    procedure sbOutputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure btnAddKnownClick(Sender: TObject);
    procedure btnAddTrustedClick(Sender: TObject);
    procedure btnRemoveKnownClick(Sender: TObject);
    procedure btnRemoveTrustedClick(Sender: TObject);
    procedure sbSignCertFileClick(Sender: TObject);
    procedure btnAddCertClick(Sender: TObject);
    procedure btnRemoveCertClick(Sender: TObject);
  private
    { Private declarations }
    FSigner: TsbxJAdESSigner;

    procedure UpdateSigningCertificates;
    procedure UpdateKnownCertificates;
    procedure UpdateTrustedCertificates;
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

procedure TFormJadessigner.UpdateSigningCertificates;
var
  Item: TListItem;
  i: Integer;
  s: string;
  CertificateManager: TsbxCertificateManager;
begin
  lvSigningCertificates.Items.BeginUpdate;
  lvSigningCertificates.Items.Clear;

  CertificateManager := TsbxCertificateManager.Create(nil);
  try
    for i := 0 to FSigner.SigningChain.Count - 1 do
    begin
      CertificateManager.Certificate := FSigner.SigningChain.Item[i];

      s := CertificateManager.Certificate.Issuer;
      if s = '' then
        s := '<unknown>';

      Item := lvSigningCertificates.Items.Add;
      Item.Caption := BinaryToString(CertificateManager.Certificate.SerialNumber);
      Item.SubItems.Add(s);
    end;
  finally
    FreeAndNil(CertificateManager);
  end;

  lvSigningCertificates.Items.EndUpdate;
end;

procedure TFormJadessigner.UpdateKnownCertificates;
var
  Item: TListItem;
  i: Integer;
  s: string;
  CertificateManager: TsbxCertificateManager;
begin
  lvKnownCertificates.Items.BeginUpdate;
  lvKnownCertificates.Items.Clear;

  CertificateManager := TsbxCertificateManager.Create(nil);
  try
    for i := 0 to FSigner.KnownCertificates.Count - 1 do
    begin
      CertificateManager.Certificate := FSigner.KnownCertificates.Item[i];

      s := CertificateManager.Certificate.Issuer;
      if s = '' then
        s := '<unknown>';

      Item := lvKnownCertificates.Items.Add;
      Item.Caption := BinaryToString(CertificateManager.Certificate.SerialNumber);
      Item.SubItems.Add(s);
    end;
  finally
    FreeAndNil(CertificateManager);
  end;

  lvKnownCertificates.Items.EndUpdate;
end;

procedure TFormJadessigner.UpdateTrustedCertificates;
var
  Item: TListItem;
  i: Integer;
  s: string;
  CertificateManager: TsbxCertificateManager;
begin
  lvTrustedCertificates.Items.BeginUpdate;
  lvTrustedCertificates.Items.Clear;

  CertificateManager := TsbxCertificateManager.Create(nil);
  try
    for i := 0 to FSigner.TrustedCertificates.Count - 1 do
    begin
      CertificateManager.Certificate := FSigner.TrustedCertificates.Item[i];

      s := CertificateManager.Certificate.Issuer;
      if s = '' then
        s := '<unknown>';

      Item := lvTrustedCertificates.Items.Add;
      Item.Caption := BinaryToString(CertificateManager.Certificate.SerialNumber);
      Item.SubItems.Add(s);
    end;
  finally
    FreeAndNil(CertificateManager);
  end;

  lvTrustedCertificates.Items.EndUpdate;
end;

procedure TFormJadessigner.btnAddCertClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
begin
  DlgOpen.Title := 'Select certificate file';
  DlgOpen.Filter := 'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX';
  if DlgOpen.Execute then
  begin
    CertificateManager := TsbxCertificateManager.Create(nil);
    try
      try
        CertificateManager.ImportFromFile(DlgOpen.Filename, InputBox('Please enter passphrase:', '',''));

        FSigner.SigningChain.Add(CertificateManager.Certificate);

        UpdateSigningCertificates;
      except
        on E: Exception do
          MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
      end;
    finally
      FreeAndNil(CertificateManager);
    end;
  end;
end;

procedure TFormJadessigner.btnAddKnownClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
begin
  DlgOpen.Title := 'Select certificate file';
  DlgOpen.Filter := 'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX';
  if DlgOpen.Execute then
  begin
    CertificateManager := TsbxCertificateManager.Create(nil);
    try
      try
        CertificateManager.ImportFromFile(DlgOpen.Filename, InputBox('Please enter passphrase:', '',''));

        FSigner.KnownCertificates.Add(CertificateManager.Certificate);

        UpdateKnownCertificates;
      except
        on E: Exception do
          MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
      end;
    finally
      FreeAndNil(CertificateManager);
    end;
  end;
end;

procedure TFormJadessigner.btnAddTrustedClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
begin
  DlgOpen.Title := 'Select certificate file';
  DlgOpen.Filter := 'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX';
  if DlgOpen.Execute then
  begin
    CertificateManager := TsbxCertificateManager.Create(nil);
    try
      try
        CertificateManager.ImportFromFile(DlgOpen.Filename, InputBox('Please enter passphrase:', '',''));

        FSigner.TrustedCertificates.Add(CertificateManager.Certificate);

        UpdateTrustedCertificates;
      except
        on E: Exception do
          MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
      end;
    finally
      FreeAndNil(CertificateManager);
    end;
  end;
end;

procedure TFormJadessigner.btnRemoveCertClick(Sender: TObject);
begin
  if Assigned(lvSigningCertificates.Selected) then
  begin
    FSigner.SigningChain.RemoveAt(lvSigningCertificates.Selected.Index);
    UpdateSigningCertificates;
  end;
end;

procedure TFormJadessigner.btnRemoveKnownClick(Sender: TObject);
begin
  if Assigned(lvKnownCertificates.Selected) then
  begin
    FSigner.KnownCertificates.RemoveAt(lvKnownCertificates.Selected.Index);
    UpdateKnownCertificates;
  end;
end;

procedure TFormJadessigner.btnRemoveTrustedClick(Sender: TObject);
begin
  if Assigned(lvTrustedCertificates.Selected) then
  begin
    FSigner.TrustedCertificates.RemoveAt(lvTrustedCertificates.Selected.Index);
    UpdateTrustedCertificates;
  end;
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
    0: FSigner.NewSignature.Level := jaslJWS;
    1: FSigner.NewSignature.Level := jaslBaselineB;
    2: FSigner.NewSignature.Level := jaslBaselineT;
    3: FSigner.NewSignature.Level := jaslBaselineLT;
    4: FSigner.NewSignature.Level := jaslBaselineLTA;
  end;

  FSigner.NewSignature.HashAlgorithm := 'SHA256';

  FSigner.IgnoreChainValidationErrors := cbIgnoreChainValidationErrors.Checked;

  if cbForceCompleteChainValidation.Checked then
    FSigner.Config('ForceCompleteChainValidation=true')
  else
    FSigner.Config('ForceCompleteChainValidation=false');

  if cbIncludeLocalRevInfo.Checked then
    FSigner.Config('IncludeKnownRevocationInfoToSignature=true')
  else
    FSigner.Config('IncludeKnownRevocationInfoToSignature=false');

  FSigner.NewSignature.PolicyID := edIdentifier.Text;
  FSigner.NewSignature.PolicyHashAlgorithm := cbHashAlgorithm.Text;
  FSigner.NewSignature.PolicyHash := edHashValue.Text;

  if cbRequestTimestamp.Checked then
  begin
    if (FSigner.NewSignature.Level = jaslBaselineB) or
       (FSigner.NewSignature.Level = jaslJWS) then
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
  dlgOpen.Filter := 'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX';
  if dlgOpen.Execute then
    edSigningCertificate.Text := dlgOpen.FileName;
end;

end.


