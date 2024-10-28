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
unit messageverifierf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxCertificateManager, SBxMessageVerifier;

type
  TFormMessageverifier = class(TForm)
    lbXMLFile: TLabel;
    sbBrowseInputFile: TSpeedButton;
    lOutputFile: TLabel;
    sbBrowseOutputFile: TSpeedButton;
    Label10: TLabel;
    cbDetached: TCheckBox;
    edInputFile: TEdit;
    btnVerify: TButton;
    edOutputFile: TEdit;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    GroupBox2: TGroupBox;
    lvKnownCertificates: TListView;
    btnRemoveKnown: TButton;
    btnAddKnown: TButton;
    procedure sbBrowseInputFileClick(Sender: TObject);
    procedure sbBrowseOutputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure cbDetachedClick(Sender: TObject);
    procedure btnAddKnownClick(Sender: TObject);
    procedure btnRemoveKnownClick(Sender: TObject);
  private
    { Private declarations }
    FVerifier: TsbxMessageVerifier;

    procedure DoSignatureFound(Sender: TObject; const IssuerRDN: string; const SerialNumber: TBytes;
      const SubjectKeyID: TBytes; CertFound: boolean; var ValidateSignature: boolean; var ValidateChain: boolean);

    procedure UpdateKnownCertificates;
    function WriteCertificateInfo(CertList: TsbxCertificateList): string;
  public
    { Public declarations }
  end;

var
  FormMessageverifier: TFormMessageverifier;

implementation

{$R *.dfm}

uses resultsf, usignf;

function BinaryToString(const Buffer : TBytes): string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to Length(Buffer) - 1 do
    Result := Result + IntToHex(Buffer[i], 1);
end;

procedure TFormMessageverifier.UpdateKnownCertificates;
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

function TFormMessageverifier.WriteCertificateInfo(CertList: TsbxCertificateList) : string;
var
  i: integer;
begin
  for i := 0 to CertList.Count - 1 do
  begin
    Result := Result + 'Certificate #' + IntToStr(i + 1) + ':'#13#10;

    Result := Result + 'Issuer: ' + CertList.Item[i].Issuer + #13#10;
    Result := Result + 'Subject: ' + CertList.Item[i].Subject + #13#10;

    if CertList.Item[i].PrivateKeyExists then
      Result := Result + 'Private key available'#13#10#13#10
    else
      Result := Result + 'Private key is not available'#13#10#13#10;
  end;
end;

procedure TFormMessageverifier.DoSignatureFound(Sender: TObject; const IssuerRDN: string; const SerialNumber: TBytes;
  const SubjectKeyID: TBytes; CertFound: boolean; var ValidateSignature: boolean; var ValidateChain: boolean);
var
  frmSign: TFormUsign;
begin
  ValidateChain := false; // not support certificate chain validation.

  if CertFound then
    ValidateSignature := true
  else
  begin
    frmSign := TFormUsign.Create(nil);
    try
      frmSign.Init(FVerifier, IssuerRDN, SerialNumber, SubjectKeyID);

      if frmSign.ShowModal = mrOK then
        ValidateSignature := true
      else
        ValidateSignature := false;
    finally
      FreeAndNil(frmSign);
    end;
  end;
end;

procedure TFormMessageverifier.btnAddKnownClick(Sender: TObject);
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

procedure TFormMessageverifier.btnRemoveKnownClick(Sender: TObject);
begin
  if Assigned(lvKnownCertificates.Selected) then
  begin
    FVerifier.KnownCertificates.RemoveAt(lvKnownCertificates.Selected.Index);
    UpdateKnownCertificates;
  end;
end;

procedure TFormMessageverifier.btnVerifyClick(Sender: TObject);
var
  SigOK: boolean;
begin
  try
    FVerifier.InputFile := edInputFile.Text;

    if cbDetached.Checked then
    begin
      FVerifier.DataFile := edOutputFile.Text;
      FVerifier.VerifyDetached;
    end
    else
    begin
      FVerifier.OutputFile := edOutputFile.Text;
      FVerifier.Verify;
    end;

    FormResults.mResults.Clear;
    case FVerifier.SignatureValidationResult of
      svtCorrupted:
        FormResults.mResults.Lines.Add('Verification error: Corrupted');
      svtFailure:
        FormResults.mResults.Lines.Add('Verification error: Failure');
      svtSignerNotFound:
        FormResults.mResults.Lines.Add('Verification error: SignerNotFound');
      svtUnknown:
        FormResults.mResults.Lines.Add('Verification error: Unknown');
      else
      begin
        FormResults.mResults.Lines.Add('Successfully verified!');
        FormResults.mResults.Lines.Add('');
        FormResults.mResults.Lines.Add('Hash Algorithm: ' + FVerifier.HashAlgorithm);
        FormResults.mResults.Lines.Add('');
        FormResults.mResults.Lines.Add('Certificates contained in message:');
        FormResults.mResults.Lines.Add(WriteCertificateInfo(FVerifier.Certificates));
      end;
    end;

    FormResults.ShowModal;
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormMessageverifier.cbDetachedClick(Sender: TObject);
begin
  if cbDetached.Checked then
    lOutputFile.Caption := 'Data file:'
  else
    lOutputFile.Caption := 'Output file:';
end;

procedure TFormMessageverifier.FormCreate(Sender: TObject);
begin
  FVerifier := TsbxMessageVerifier.Create(nil);
end;

procedure TFormMessageverifier.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FVerifier);
end;

procedure TFormMessageverifier.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edInputFile.Text;
  dlgOpen.Filter := '';
  if dlgOpen.Execute then
    edInputFile.Text := dlgOpen.FileName;
end;

procedure TFormMessageverifier.sbBrowseOutputFileClick(Sender: TObject);
begin
  if cbDetached.Checked then
  begin
    dlgOpen.FileName := edOutputFile.Text;
    dlgOpen.Filter := '';
    if dlgOpen.Execute then
      edOutputFile.Text := dlgOpen.FileName;
  end
  else
  begin
    dlgSave.FileName := edOutputFile.Text;
    if dlgSave.Execute then
      edOutputFile.Text := dlgSave.FileName;
  end;
end;

end.






