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
unit xadesverifierf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxCertificateManager, SBxXAdESVerifier;

type
  TFormXadesverifier = class(TForm)
    lbXMLFile: TLabel;
    sbBrowseInputFile: TSpeedButton;
    lDataFile: TLabel;
    sbBrowseDataFile: TSpeedButton;
    Label10: TLabel;
    cbDetached: TCheckBox;
    edInputFile: TEdit;
    btnVerify: TButton;
    edDataFile: TEdit;
    dlgOpen: TOpenDialog;
    FVerifier: TsbxXAdESVerifier;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure cbDetachedClick(Sender: TObject);
    procedure sbBrowseInputFileClick(Sender: TObject);
    procedure sbBrowseDataFileClick(Sender: TObject);
  private
    { Private declarations }

    procedure DoReferenceValidated(Sender: TObject; ReferenceIndex: Integer;
      const Id: string; const Uri: string; const RefType: string; DigestValid: boolean);
    procedure DoSignatureFound(Sender: TObject; SigIndex : Integer; const EntityLabel: String;
      const IssuerRDN: string; const SerialNumber: TBytes; const SubjectKeyID: TBytes;
      CertFound: boolean; var ValidateSignature: boolean; var ValidateChain: boolean);
    procedure DoTimestampValidated(Sender: TObject; Index: Integer; const EntityLabel: String;
      const IssuerRDN: String; const SerialNumber: TBytes; const SubjectKeyID: TBytes; const Time: String;
      ValidationResult: Integer; ChainValidationResult: Integer; ChainValidationDetails: Integer; var Cancel: Boolean);
  public
    { Public declarations }
  end;

var
  FormXadesverifier: TFormXadesverifier;

implementation

{$R *.dfm}

uses referencesf, xadesf, usignf;

function BinaryToString(const Buffer : TBytes): string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to Length(Buffer) - 1 do
    Result := Result + IntToHex(Buffer[i], 1);
end;

procedure TFormXadesverifier.DoReferenceValidated(Sender: TObject; ReferenceIndex: Integer;
  const Id: string; const Uri: string; const RefType: string; DigestValid: boolean);
var
  Item: TListItem;
begin
  FormReferences.lvReferenceResults.Items.BeginUpdate;

  Item := FormReferences.lvReferenceResults.Items.Add;
  Item.Caption := Id;
  Item.SubItems.Add(Uri);
  if DigestValid then
    Item.SubItems.Add('true')
  else
    Item.SubItems.Add('false');

  FormReferences.lvReferenceResults.Items.EndUpdate;
end;

procedure TFormXadesverifier.DoSignatureFound(Sender: TObject; SigIndex : Integer;
  const EntityLabel: String; const IssuerRDN: string; const SerialNumber: TBytes; const SubjectKeyID: TBytes;
  CertFound: boolean; var ValidateSignature: boolean; var ValidateChain: boolean);
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
      frmSign.SetSignature(FVerifier, SigIndex);

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

procedure TFormXadesverifier.DoTimestampValidated(Sender: TObject; Index: Integer; const EntityLabel: String;
  const IssuerRDN: String; const SerialNumber: TBytes; const SubjectKeyID: TBytes; const Time: String;
  ValidationResult: Integer; ChainValidationResult: Integer; ChainValidationDetails: Integer; var Cancel: Boolean);
begin
  if ValidationResult = 0 {cvtValid} then
  begin
    FormXades.lbTimestamp.Caption := 'Timestamp: ' + Time;
    FormXades.lbTimestampSerial.Caption := 'Timestamp Serial: ' + BinaryToString(SerialNumber);
  end
  else
  if ValidationResult = 1 {cvtValidButUntrusted} then
    FormXades.lbTimestamp.Caption := 'Timestamp is signed by self-signed certificate which was not previously trusted'
  else
  if ValidationResult = 3 {cvtCantBeEstablished} then
    FormXades.lbTimestamp.Caption := 'Timestamp signing certificate chain could not be validated completely'
  else
    FormXades.lbTimestamp.Caption := 'Timestamp signature is not valid';
end;

procedure TFormXadesverifier.btnVerifyClick(Sender: TObject);
var
  I : Integer;
  SigOK: boolean;
begin
  try
    FormReferences.lvReferenceResults.Items.BeginUpdate;
    FormReferences.lvReferenceResults.Items.Clear;
    FormReferences.lvReferenceResults.Items.EndUpdate;

    FormXades.lbTimestamp.Caption := '';
    FormXades.lbTimestampSerial.Caption := '';

    FVerifier.KnownCertificates.Clear;

    FVerifier.InputFile := edInputFile.Text;

    if cbDetached.Checked then
    begin
      FVerifier.DataFile := edDataFile.Text;
      FVerifier.DataType := cxdtBinary;
      FVerifier.DataURI := ExtractFileName(edDataFile.Text);
      FVerifier.VerifyDetached;
    end
    else
      FVerifier.Verify;

    if FVerifier.Signatures.Count = 0 then
      MessageDlg('No signatures found', mtError, [mbOk], 0)
    else
    begin
      for I := 0 to FVerifier.Signatures.Count - 1 do
      begin
        case FVerifier.Signatures[I].SignatureValidationResult of
          svtSignerNotFound:
            MessageDlg('Signer not found', mtError, [mbOk], 0);
          svtFailure:
            MessageDlg('Signature verification failed', mtError, [mbOk], 0);
          svtCorrupted:
            MessageDlg('Signature is invalid', mtError, [mbOk], 0);
          svtReferenceCorrupted:
            if MessageDlg('Signature has invalid references.'#13#10'Do you want to view reference validation results?', mtError, [mbYes, mbNo], 0) = mrYes then
              FormReferences.ShowModal;
          svtValid:
          begin
            SigOK := true;

            if FVerifier.Signatures[I].ChainValidationResult = cvtValidButUntrusted then
              MessageDlg('The selected signature is signed by self-signed certificate which was not previously trusted', mtInformation, [mbOk], 0)
            else
            if FVerifier.Signatures[I].ChainValidationResult <> cvtValid then
            begin
              //SigOK := false;
              if FVerifier.Signatures[I].ChainValidationResult = cvtCantBeEstablished then
                MessageDlg('Signing certificate chain could not be validated completely', mtError, [mbOk], 0)
              else
                MessageDlg('Signing certificate is not valid', mtError, [mbOk], 0);
            end;

            if SigOK then
            begin
              if MessageDlg('Signature validated successfully.'#13#10'Do you want to view reference validation results?', mtInformation, [mbYes, mbNo], 0) = mrYes then
                FormReferences.ShowModal;

              if FVerifier.Signatures[I].Level <> TsbxAdESSignatureLevels.aslGeneric then
                if MessageDlg('Do you want to view XAdES information?', mtInformation, [mbYes, mbNo], 0) = mrYes then
                begin
                  FormXades.Initialize(FVerifier, I);
                  FormXades.ShowModal;
                end;
            end;
          end;
          else
            MessageDlg('Unknown signature validation result', mtInformation, [mbOk], 0);
        end;
      end;
    end;
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormXadesverifier.cbDetachedClick(Sender: TObject);
begin
  lDataFile.Enabled := cbDetached.Checked;
  edDataFile.Enabled := cbDetached.Checked;
  sbBrowseDataFile.Enabled := cbDetached.Checked;
end;

procedure TFormXadesverifier.FormCreate(Sender: TObject);
begin
  FVerifier := TsbxXAdESVerifier.Create(nil);
  FVerifier.OnReferenceValidated := DoReferenceValidated;
  FVerifier.OnSignatureFound := DoSignatureFound;
  FVerifier.OnTimestampValidated := DoTimestampValidated;
  lDataFile.Enabled := cbDetached.Checked;
end;

procedure TFormXadesverifier.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FVerifier);
end;

procedure TFormXadesverifier.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edInputFile.Text;
  if dlgOpen.Execute then
    edInputFile.Text := dlgOpen.FileName;
end;

procedure TFormXadesverifier.sbBrowseDataFileClick(Sender: TObject);
begin
  dlgOpen.InitialDir := ExtractFilePath(Application.ExeName);
  if dlgOpen.Execute then
    edDataFile.Text := dlgOpen.FileName;
end;

end.
