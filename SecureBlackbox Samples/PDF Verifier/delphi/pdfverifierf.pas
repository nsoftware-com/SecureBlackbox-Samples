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
unit pdfverifierf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxCertificateManager, SBxPDFVerifier;

type
  TFormPdfverifier = class(TForm)
    lbXMLFile: TLabel;
    sbBrowseInputFile: TSpeedButton;
    Label10: TLabel;
    edInputFile: TEdit;
    btnVerify: TButton;
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
    cbIgnoreChainValidationErrors: TCheckBox;
    cbPerformRevocationCheck: TCheckBox;
    cbForceCompleteChainValidation: TCheckBox;
    cbOfflineMode: TCheckBox;
    procedure sbBrowseInputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure btnAddKnownClick(Sender: TObject);
    procedure btnRemoveKnownClick(Sender: TObject);
    procedure btnAddTrustedClick(Sender: TObject);
    procedure btnRemoveTrustedClick(Sender: TObject);
  private
    { Private declarations }
    FVerifier: TsbxPDFVerifier;

    procedure UpdateKnownCertificates;
    procedure UpdateTrustedCertificates;
    procedure DoSignatureFound(Sender: TObject; index: integer; const EntityLabel: String;
      const IssuerRDN: string; const SerialNumber: TBytes; const SubjectKeyID: TBytes; CertFound: Boolean;
      var ValidateSignature: Boolean; var ValidateChain: Boolean);
  public
    { Public declarations }
  end;

var
  FormPdfverifier: TFormPdfverifier;

implementation

{$R *.dfm}

uses usignf, validationresultf;

function BinaryToString(const Buffer : TBytes): string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to Length(Buffer) - 1 do
    Result := Result + IntToHex(Buffer[i], 1);
end;

procedure TFormPdfverifier.UpdateKnownCertificates;
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

procedure TFormPdfverifier.UpdateTrustedCertificates;
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

procedure TFormPdfverifier.btnAddKnownClick(Sender: TObject);
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

procedure TFormPdfverifier.btnAddTrustedClick(Sender: TObject);
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

procedure TFormPdfverifier.btnRemoveKnownClick(Sender: TObject);
begin
  if Assigned(lvKnownCertificates.Selected) then
  begin
    FVerifier.KnownCertificates.RemoveAt(lvKnownCertificates.Selected.Index);
    UpdateKnownCertificates;
  end;
end;

procedure TFormPdfverifier.btnRemoveTrustedClick(Sender: TObject);
begin
  if Assigned(lvTrustedCertificates.Selected) then
  begin
    FVerifier.TrustedCertificates.RemoveAt(lvTrustedCertificates.Selected.Index);
    UpdateTrustedCertificates;
  end;
end;

procedure TFormPdfverifier.DoSignatureFound(Sender: TObject; index: integer; const EntityLabel: String;
  const IssuerRDN: string; const SerialNumber: TBytes; const SubjectKeyID: TBytes; CertFound: Boolean;
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

procedure TFormPdfverifier.btnVerifyClick(Sender: TObject);
begin
  try
    FVerifier.InputFile := edInputFile.Text;

    if cbPerformRevocationCheck.Checked then
      FVerifier.RevocationCheck := TsbxPDFVerifierRevocationChecks.crcAuto
    else
      FVerifier.RevocationCheck := TsbxPDFVerifierRevocationChecks.crcNone;

    FVerifier.IgnoreChainValidationErrors := cbIgnoreChainValidationErrors.Checked;

    if cbForceCompleteChainValidation.Checked then
      FVerifier.Config('ForceCompleteChainValidation=true')
    else
      FVerifier.Config('ForceCompleteChainValidation=False');

    FVerifier.OfflineMode := cbOfflineMode.Checked;

    FVerifier.Verify;

    FormValidationresult.Init(FVerifier);
    FormValidationresult.ShowModal;
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormPdfverifier.FormCreate(Sender: TObject);
begin
  FVerifier := TsbxPDFVerifier.Create(nil);
  FVerifier.OnSignatureFound := DoSignatureFound;
end;

procedure TFormPdfverifier.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FVerifier);
end;

procedure TFormPdfverifier.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edInputFile.Text;
  dlgOpen.Filter := 'PDF files (*.pdf)|*.pdf|All files (*.*)|*.*';
  if dlgOpen.Execute then
    edInputFile.Text := dlgOpen.FileName;
end;

end.
