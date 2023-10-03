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
    GroupBox5: TGroupBox;
    lvSigningCertificates: TListView;
    btnRemoveCert: TButton;
    btnAddCert: TButton;
    cbLevel: TComboBox;
    cbVisible: TCheckBox;
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
    cbIncludeRevocationInfoToAdbeAttribute: TCheckBox;
    btnSign: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    GroupBox3: TGroupBox;
    lAuthorName: TLabel;
    editAuthorName: TEdit;
    lReason: TLabel;
    cbReason: TComboBox;
    cbAutoCollectRevInfo: TCheckBox;
    cbIgnoreChainValidationErrors: TCheckBox;
    cbForceCompleteChainValidation: TCheckBox;
    cbDeepValidation: TCheckBox;
    editTSPServer: TEdit;
    cbRequestTimestamp: TCheckBox;
    GroupBox7: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    Label11: TLabel;
    cbHashAlgorithm: TComboBox;
    edIdentifier: TEdit;
    edHashValue: TEdit;
    procedure sbBrowseInputFileClick(Sender: TObject);
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
    FSigner: TsbxPDFSigner;

    procedure UpdateSigningCertificates;
    procedure UpdateKnownCertificates;
    procedure UpdateTrustedCertificates;
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

procedure TFormPdfsigner.UpdateSigningCertificates;
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

procedure TFormPdfsigner.UpdateKnownCertificates;
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

procedure TFormPdfsigner.UpdateTrustedCertificates;
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

procedure TFormPdfsigner.btnAddCertClick(Sender: TObject);
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

procedure TFormPdfsigner.btnAddKnownClick(Sender: TObject);
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

procedure TFormPdfsigner.btnAddTrustedClick(Sender: TObject);
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

procedure TFormPdfsigner.btnRemoveCertClick(Sender: TObject);
begin
  if Assigned(lvSigningCertificates.Selected) then
  begin
    FSigner.SigningChain.RemoveAt(lvSigningCertificates.Selected.Index);
    UpdateSigningCertificates;
  end;
end;

procedure TFormPdfsigner.btnRemoveKnownClick(Sender: TObject);
begin
  if Assigned(lvKnownCertificates.Selected) then
  begin
    FSigner.KnownCertificates.RemoveAt(lvKnownCertificates.Selected.Index);
    UpdateKnownCertificates;
  end;
end;

procedure TFormPdfsigner.btnRemoveTrustedClick(Sender: TObject);
begin
  if Assigned(lvTrustedCertificates.Selected) then
  begin
    FSigner.TrustedCertificates.RemoveAt(lvTrustedCertificates.Selected.Index);
    UpdateTrustedCertificates;
  end;
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

  case cbLevel.ItemIndex of
    0: FSigner.NewSignature.Level := pslLegacy;
    1: FSigner.NewSignature.Level := pslBES;
    2: FSigner.NewSignature.Level := pslEPES;
    3: FSigner.NewSignature.Level := pslLTV;
    4: FSigner.NewSignature.Level := pslDocumentTimestamp;
  end;

  FSigner.Widget.Invisible := not cbVisible.Checked;

  FSigner.IgnoreChainValidationErrors := cbIgnoreChainValidationErrors.Checked;

  if cbAutoCollectRevInfo.Checked then
    FSigner.Config('AutoCollectRevocationInfo=true')
  else
    FSigner.Config('AutoCollectRevocationInfo=false');

  if cbForceCompleteChainValidation.Checked then
    FSigner.Config('ForceCompleteChainValidation=true')
  else
    FSigner.Config('ForceCompleteChainValidation=false');

  if cbDeepValidation.Checked then
    FSigner.Config('DeepValidation=true')
  else
    FSigner.Config('DeepValidation=false');

  if cbIncludeLocalRevInfo.Checked then
    FSigner.Config('IncludeKnownRevocationInfoToSignature=true')
  else
    FSigner.Config('IncludeKnownRevocationInfoToSignature=false');

  if cbIncludeRevocationInfoToAdbeAttribute.Checked then
    FSigner.Config('IncludeRevocationInfoToAdbeAttribute=true')
  else
    FSigner.Config('IncludeRevocationInfoToAdbeAttribute=false');

  FSigner.NewSignature.PolicyID := edIdentifier.Text;
  FSigner.NewSignature.PolicyHashAlgorithm := cbHashAlgorithm.Text;
  FSigner.NewSignature.PolicyHash := edHashValue.Text;

  FSigner.NewSignature.AuthorName := {$ifndef UNICODE}MultiByteToUTF8{$endif}(editAuthorName.Text);
  if CompareStr(cbReason.Text, '<none>') <> 0 then
    FSigner.NewSignature.Reason := {$ifndef UNICODE}MultiByteToUTF8{$endif}(cbReason.Text)
  else
    FSigner.NewSignature.Reason := '';

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
  dlgOpen.Filter := '';
  if dlgOpen.Execute then
    edInputFile.Text := dlgOpen.FileName;
end;

procedure TFormPdfsigner.sbOutputFileClick(Sender: TObject);
begin
  dlgSave.InitialDir := ExtractFilePath(Application.ExeName);
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







