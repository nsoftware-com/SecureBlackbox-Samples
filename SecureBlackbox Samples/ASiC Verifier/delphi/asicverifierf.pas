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
unit asicverifierf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls, FileCtrl,
  SBxTypes, SBxCore, SBxCertificateManager, SBxASiCVerifier;

type
  TFormAsicverifier = class(TForm)
    lbXMLFile: TLabel;
    edInputFile: TEdit;
    sbBrowseInputFile: TSpeedButton;
    btnVerify: TButton;
    DlgOpen: TOpenDialog;
    Label1: TLabel;
    sbBrowseOutputPath: TSpeedButton;
    edOutputPath: TEdit;
    Label10: TLabel;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    cbPerformRevocationCheck: TCheckBox;
    cbIgnoreChainValidationErrors: TCheckBox;
    cbForceCompleteChainValidation: TCheckBox;
    cbExtractionMode: TComboBox;
    GroupBox2: TGroupBox;
    lvKnownCertificates: TListView;
    btnRemoveKnown: TButton;
    btnAddKnown: TButton;
    GroupBox4: TGroupBox;
    lvTrustedCertificates: TListView;
    btnRemoveTrusted: TButton;
    btnAddTrusted: TButton;
    cbOfflineMode: TCheckBox;
    procedure sbBrowseInputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure btnAddKnownClick(Sender: TObject);
    procedure btnRemoveKnownClick(Sender: TObject);
    procedure btnAddTrustedClick(Sender: TObject);
    procedure btnRemoveTrustedClick(Sender: TObject);
    procedure sbBrowseOutputPathClick(Sender: TObject);
  private
    { Private declarations }
    FVerifier: TsbxASiCVerifier;

    procedure UpdateKnownCertificates;
    procedure UpdateTrustedCertificates;
    procedure DoSignatureFound(Sender: TObject; index: Integer; const EntityLabel: String;
      const IssuerRDN: string; const SerialNumber: TBytes; const SubjectKeyID: TBytes; CertFound: Boolean;
      var ValidateSignature: Boolean; var ValidateChain: Boolean);
  public
    { Public declarations }
  end;

var
  FormAsicverifier: TFormAsicverifier;

implementation

{$R *.dfm}

uses ValidationResultF, usignf;

function BinaryToString(const Buffer : TBytes): string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to Length(Buffer) - 1 do
    Result := Result + IntToHex(Buffer[i], 1);
end;

procedure TFormAsicverifier.UpdateKnownCertificates;
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

procedure TFormAsicverifier.UpdateTrustedCertificates;
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

procedure TFormAsicverifier.btnAddKnownClick(Sender: TObject);
var
  Certificatemanager: TsbxCertificatemanager;
begin
  DlgOpen.Title := 'Select certificate file';
  DlgOpen.Filter := 'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX';
  if DlgOpen.Execute then
  begin
    Certificatemanager := TsbxCertificatemanager.Create(nil);
    try
      try
        Certificatemanager.ImportFromFile(DlgOpen.Filename, InputBox('Please enter passphrase:', '',''));

        FVerifier.KnownCertificates.Add(Certificatemanager.Certificate);

        UpdateKnownCertificates;
      except
        on E: Exception do
          MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
      end;
    finally
      FreeAndNil(Certificatemanager);
    end;
  end;
end;

procedure TFormAsicverifier.btnAddTrustedClick(Sender: TObject);
var
  Certificatemanager: TsbxCertificatemanager;
begin
  DlgOpen.Title := 'Select certificate file';
  DlgOpen.Filter := 'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX';
  if DlgOpen.Execute then
  begin
    Certificatemanager := TsbxCertificatemanager.Create(nil);
    try
      try
        Certificatemanager.ImportFromFile(DlgOpen.Filename, InputBox('Please enter passphrase:', '',''));

        FVerifier.TrustedCertificates.Add(Certificatemanager.Certificate);

        UpdateTrustedCertificates;
      except
        on E: Exception do
          MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
      end;
    finally
      FreeAndNil(Certificatemanager);
    end;
  end;
end;

procedure TFormAsicverifier.btnRemoveKnownClick(Sender: TObject);
begin
  if Assigned(lvKnownCertificates.Selected) then
  begin
    FVerifier.KnownCertificates.RemoveAt(lvKnownCertificates.Selected.Index);
    UpdateKnownCertificates;
  end;
end;

procedure TFormAsicverifier.btnRemoveTrustedClick(Sender: TObject);
begin
  if Assigned(lvTrustedCertificates.Selected) then
  begin
    FVerifier.TrustedCertificates.RemoveAt(lvTrustedCertificates.Selected.Index);
    UpdateTrustedCertificates;
  end;
end;

procedure TFormAsicverifier.DoSignatureFound(Sender: TObject; index: Integer; const EntityLabel: String;
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



procedure TFormAsicverifier.btnVerifyClick(Sender: TObject);
begin
  try
    FVerifier.InputFile := edInputFile.Text;
    FVerifier.OutputPath := edOutputPath.Text;

    case cbExtractionMode.ItemIndex of
      1: FVerifier.ExtractionMode := aemAll;
      2: FVerifier.ExtractionMode := aemSigned;
      3: FVerifier.ExtractionMode := aemSignedAndValid;
      else FVerifier.ExtractionMode := aemNone;
    end;

    FVerifier.OfflineMode := cbOfflineMode.Checked;

    if cbPerformRevocationCheck.Checked then
      FVerifier.RevocationCheck := crcAuto
    else
      FVerifier.RevocationCheck := crcNone;

    FVerifier.IgnoreChainValidationErrors := cbIgnoreChainValidationErrors.Checked;

    if cbForceCompleteChainValidation.Checked then
      FVerifier.Config('ForceCompleteChainValidation=true')
    else
      FVerifier.Config('ForceCompleteChainValidation=False');

    FVerifier.Verify;

    FormValidationresult.Init(FVerifier);
    FormValidationresult.ShowModal;
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormAsicverifier.FormCreate(Sender: TObject);
begin
  FVerifier := TsbxASiCVerifier.Create(nil);
  FVerifier.OnSignatureFound := DoSignatureFound;
end;

procedure TFormAsicverifier.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FVerifier);
end;

procedure TFormAsicverifier.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edInputFile.Text;
  dlgOpen.Filter := '';
  if dlgOpen.Execute then
    edInputFile.Text := dlgOpen.FileName;
end;

procedure TFormAsicverifier.sbBrowseOutputPathClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := edOutputPath.Text;
  if SelectDirectory(Dir, [], 0) then
    edOutputPath.Text := Dir;
end;

end.
