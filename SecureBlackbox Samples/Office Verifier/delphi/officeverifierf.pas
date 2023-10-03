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
unit officeverifierf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxCertificateManager, SBxOfficeVerifier;

type
  TFormOfficeverifier = class(TForm)
    lbXMLFile: TLabel;
    sbBrowseInputFile: TSpeedButton;
    Label10: TLabel;
    edInputFile: TEdit;
    btnVerify: TButton;
    GroupBox3: TGroupBox;
    cbPerformRevocationCheck: TCheckBox;
    cbIgnoreChainValidationErrors: TCheckBox;
    cbForceCompleteChainValidation: TCheckBox;
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
    FVerifier: TsbxOfficeVerifier;

    procedure UpdateKnownCertificates;
    procedure UpdateTrustedCertificates;
    procedure DoSignatureFound(Sender: TObject; SigIndex : Integer; const IssuerRDN: String; SerialNumber: TBytes;
      SubjectKeyID: TBytes; CertFound: Boolean; var ValidateSignature: Boolean; var ValidateChain: Boolean);
  public
    { Public declarations }
  end;

var
  FormOfficeverifier: TFormOfficeverifier;

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

procedure TFormOfficeverifier.UpdateKnownCertificates;
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

procedure TFormOfficeverifier.UpdateTrustedCertificates;
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

procedure TFormOfficeverifier.btnAddKnownClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
begin
  dlgOpen.Title := 'Select certificate file';
  dlgOpen.Filter := 'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX';
  if dlgOpen.Execute then
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

procedure TFormOfficeverifier.btnAddTrustedClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
begin
  dlgOpen.Title := 'Select certificate file';
  dlgOpen.Filter := 'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX';
  if dlgOpen.Execute then
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

procedure TFormOfficeverifier.btnRemoveKnownClick(Sender: TObject);
begin
  if Assigned(lvKnownCertificates.Selected) then
  begin
    FVerifier.KnownCertificates.RemoveAt(lvKnownCertificates.Selected.Index);
    UpdateKnownCertificates;
  end;
end;

procedure TFormOfficeverifier.btnRemoveTrustedClick(Sender: TObject);
begin
  if Assigned(lvTrustedCertificates.Selected) then
  begin
    FVerifier.TrustedCertificates.RemoveAt(lvTrustedCertificates.Selected.Index);
    UpdateTrustedCertificates;
  end;
end;

procedure TFormOfficeverifier.DoSignatureFound(Sender: TObject;
  SigIndex : Integer; const IssuerRDN: String; SerialNumber: TBytes; SubjectKeyID: TBytes;
  CertFound: Boolean; var ValidateSignature: Boolean; var ValidateChain: Boolean);
var
  frmSign: TFormUsign;
begin
  if not CertFound then
  begin
    frmSign := TFormUsign.Create(nil);
    try
      frmSign.Init(FVerifier);

      frmSign.ShowModal;
    finally
      FreeAndNil(frmSign);
    end;
  end;
end;

procedure TFormOfficeverifier.btnVerifyClick(Sender: TObject);
begin
  try
    FVerifier.InputFile := edInputFile.Text;

    if cbPerformRevocationCheck.Checked then
      FVerifier.RevocationCheck := TsbxofficeverifierRevocationChecks.crcAuto
    else
      FVerifier.RevocationCheck := TsbxofficeverifierRevocationChecks.crcNone;

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

procedure TFormOfficeverifier.FormCreate(Sender: TObject);
begin
  FVerifier := TsbxOfficeVerifier.Create(nil);
  FVerifier.OnSignatureFound := DoSignatureFound;
end;

procedure TFormOfficeverifier.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FVerifier);
end;

procedure TFormOfficeverifier.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edInputFile.Text;
  dlgOpen.Filter :=       
      'All Documents|*.doc; *.dot; *.docx; *.docm; *.dotx; *.dotm; *.xl' +
      's; *.xlt; *.xlm; *.xlsx; *.xlsm; *.xlsb; *.xltx; *.xltm; *.ppt; ' +
      '*.ppm; *.pot; *.pps; *.pptx; *.pptm; *.potm; *.ppsx; *.ppsm; *.x' +
      'ps; *.odt; *.ott; *.odg; *.otg; *.odp; *.otp; *.ods; *.ots; *.od' +
      'c; *.otc; *.odi; *.oti; *.odf; *.otf; *.odm; *.oth; *.odb|All fi' +
      'les (*.*)|*.*|Word Document (*.doc; *.dot; *.docx; *.docm; *.dot' +
      'x; *.dotm)|*.doc; *.dot; *.docx; *.docm; *.dotx; *.dotm|Excel (*' +
      '.xls; *.xlt; *.xlm; *.xlsx; *.xlsm; *.xlsb; *.xltx; *.xltm)|*.xl' +
      's; *.xlt; *.xlm; *.xlsx; *.xlsm; *.xlsb; *.xltx; *.xltm|PowerPoi' +
      'nt (*.ppt; *.ppm; *.pot; *.pps; *.pptx; *.pptm; *.potm; *.ppsx; ' +
      '*.ppsm)|*.ppt; *.ppm; *.pot; *.pps; *.pptx; *.pptm; *.potm; *.pp' +
      'sx; *.ppsm|Open XPS (*.xps)|*.xps|OpenDocument Format|*.odt; *.o' +
      'tt; *.odg; *.otg; *.odp; *.otp; *.ods; *.ots; *.odc; *.otc; *.od' +
      'i; *.oti; *.odf; *.otf; *.odm; *.oth; *.odb';

  if dlgOpen.Execute then
    edInputFile.Text := dlgOpen.FileName;
end;

end.

