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
unit cadesverifierf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxCertificateManager, SBxCAdESVerifier;

type
  TFormCadesverifier = class(TForm)
    cbDetached: TCheckBox;
    lbXMLFile: TLabel;
    sbBrowseInputFile: TSpeedButton;
    lOutputFile: TLabel;
    sbBrowseOutputFile: TSpeedButton;
    Label10: TLabel;
    edInputFile: TEdit;
    btnVerify: TButton;
    edOutputFile: TEdit;
    GroupBox3: TGroupBox;
    GroupBox2: TGroupBox;
    lvKnownCertificates: TListView;
    btnRemoveKnown: TButton;
    btnAddKnown: TButton;
    GroupBox4: TGroupBox;
    lvTrustedCertificates: TListView;
    btnRemoveTrusted: TButton;
    btnAddTrusted: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    cbIgnoreChainValidationErrors: TCheckBox;
    cbPerformRevocationCheck: TCheckBox;
    cbForceCompleteChainValidation: TCheckBox;
    cbOfflineMode: TCheckBox;
    procedure sbBrowseInputFileClick(Sender: TObject);
    procedure sbBrowseOutputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure cbDetachedClick(Sender: TObject);
    procedure btnAddKnownClick(Sender: TObject);
    procedure btnAddTrustedClick(Sender: TObject);
    procedure btnRemoveKnownClick(Sender: TObject);
    procedure btnRemoveTrustedClick(Sender: TObject);
  private
    { Private declarations }
    FVerifier: TsbxCAdESVerifier;

    procedure UpdateKnownCertificates;
    procedure UpdateTrustedCertificates;
    procedure DoSignatureFound(Sender: TObject; Index: Integer; const EntityLabel: String;
      const IssuerRDN: String; const SerialNumber: TBytes; const SubjectKeyID: TBytes; CertFound: Boolean;
      var ValidateSignature: Boolean; var ValidateChain: Boolean);
  public
    { Public declarations }
  end;

var
  FormCadesverifier: TFormCadesverifier;

implementation

{$R *.dfm}

uses uSignf;

function BinaryToString(const Buffer : TBytes): string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to Length(Buffer) - 1 do
    Result := Result + IntToHex(Buffer[i], 1);
end;

procedure TFormCadesverifier.UpdateKnownCertificates;
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
    for i := 0 to FVerifier.KnownCertificates.Count - 1 do
    begin
      CertificateManager.Certificate := FVerifier.KnownCertificates.Item[i];

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

procedure TFormCadesverifier.UpdateTrustedCertificates;
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
    for i := 0 to FVerifier.TrustedCertificates.Count - 1 do
    begin
      CertificateManager.Certificate := FVerifier.TrustedCertificates.Item[i];

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

procedure TFormCadesverifier.DoSignatureFound(Sender: TObject; Index: Integer; const EntityLabel: String;
  const IssuerRDN: String; const SerialNumber: TBytes; const SubjectKeyID: TBytes; CertFound: Boolean;
  var ValidateSignature: Boolean; var ValidateChain: Boolean);
var
  frmSign: TFormUsign;
begin
  if CertFound then
  begin
    ValidateSignature := true;
    ValidateChain := true;
  end
  else
  begin
    frmSign := TFormUsign.Create(nil);
    try
      frmSign.Init(FVerifier, IssuerRDN, SerialNumber, SubjectKeyID);

      if frmSign.ShowModal = mrOK then
      begin
        ValidateSignature := true;
        ValidateChain := true;
      end
      else
      begin
        ValidateSignature := false;
        ValidateChain := false;
      end;
    finally
      FreeAndNil(frmSign);
    end;
  end;
end;

procedure TFormCadesverifier.btnAddKnownClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
begin
  DlgOpen.Title := 'Select certificate file';
  DlgOpen.Filter := 'Certificates (*.pem, *.cer, *.crt, *.der, *.pfx, *.p12, *.pkcs12)|*.pem;*.cer;*.crt;*.der;*.pfx;*.p12;*.pkcs12|PEM-encoded certificates (*.pem)|*.pem|' +
    'DER-encoded certificates (*.cer, *.crt, *.der)|*.cer;*.crt;*.der|PKCS#12 encoded certificates (*.pfx, *.p12, *.pkcs12)|*.pfx;*.p12;*.pkcs12|All files (*.*)|*.*';
  if DlgOpen.Execute then
  begin
    CertificateManager := TsbxCertificateManager.Create(nil);
    try
      try
        CertificateManager.ImportFromFile(DlgOpen.Filename, InputBox('Please enter passphrase:', '',''));

        FVerifier.KnownCertificates.Add(CertificateManager.Certificate);

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

procedure TFormCadesverifier.btnAddTrustedClick(Sender: TObject);
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

        FVerifier.TrustedCertificates.Add(CertificateManager.Certificate);

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

procedure TFormCadesverifier.btnRemoveKnownClick(Sender: TObject);
begin
  if Assigned(lvKnownCertificates.Selected) then
  begin
    FVerifier.KnownCertificates.RemoveAt(lvKnownCertificates.Selected.Index);
    UpdateKnownCertificates;
  end;
end;

procedure TFormCadesverifier.btnRemoveTrustedClick(Sender: TObject);
begin
  if Assigned(lvTrustedCertificates.Selected) then
  begin
    FVerifier.TrustedCertificates.RemoveAt(lvTrustedCertificates.Selected.Index);
    UpdateTrustedCertificates;
  end;
end;

procedure TFormCadesverifier.btnVerifyClick(Sender: TObject);
begin
  try
    FVerifier.InputFile := edInputFile.Text;

    if cbPerformRevocationCheck.Checked then
      FVerifier.RevocationCheck := crcAuto
    else
      FVerifier.RevocationCheck := crcNone;

    FVerifier.IgnoreChainValidationErrors := cbIgnoreChainValidationErrors.Checked;

    if cbForceCompleteChainValidation.Checked then
      FVerifier.Config('ForceCompleteChainValidation=true')
    else
      FVerifier.Config('ForceCompleteChainValidation=False');

    if cbDetached.Checked then
      FVerifier.DataFile := edOutputFile.Text
    else
      FVerifier.OutputFile := edOutputFile.Text;

    FVerifier.Detached := cbDetached.Checked;
    FVerifier.OfflineMode :=  cbOfflineMode.Checked;

    FVerifier.Verify;

    case FVerifier.Signatures.Item[0].SignatureValidationResult of
      svtSignerNotFound:
        MessageDlg('Signer not found', mtError, [mbOk], 0);
      svtFailure:
        MessageDlg('Signature verification failed', mtError, [mbOk], 0);
      svtCorrupted:
        MessageDlg('Signature is invalid', mtError, [mbOk], 0);
      else
        MessageDlg('Signature validated successfully.', mtInformation, [mbOk], 0);
    end;
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormCadesverifier.cbDetachedClick(Sender: TObject);
begin
  if cbDetached.Checked then
    lOutputFile.Caption := 'Data file:'
  else 
    lOutputFile.Caption := 'Output file:';
end;

procedure TFormCadesverifier.FormCreate(Sender: TObject);
begin
  FVerifier := TsbxCAdESVerifier.Create(nil);
  FVerifier.OnSignatureFound := DoSignatureFound;
end;

procedure TFormCadesverifier.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FVerifier);
end;

procedure TFormCadesverifier.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpen.InitialDir := ExtractFilePath(Application.ExeName);
  dlgOpen.Filter := '';
  if dlgOpen.Execute then
    edInputFile.Text := dlgOpen.FileName;
end;

procedure TFormCadesverifier.sbBrowseOutputFileClick(Sender: TObject);
begin
  if cbDetached.Checked then
  begin
    dlgOpen.InitialDir := ExtractFilePath(Application.ExeName);
    dlgOpen.Filter := '';
    if dlgOpen.Execute then
      edOutputFile.Text := dlgOpen.FileName;
  end
  else
  begin
    dlgSave.InitialDir := ExtractFilePath(Application.ExeName);
    if dlgSave.Execute then
      edOutputFile.Text := dlgSave.FileName;
  end;
end;

end.




