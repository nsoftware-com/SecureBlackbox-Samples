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
unit authenticodeverifierf;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  sbxcore, sbxtypes, sbxauthenticodeverifier, sbxcertificatemanager;

type
  TFormAuthenticodeverifier = class(TForm)
    lblInfo: TLabel;
    lblInputFile: TLabel;
    edtInputFile: TEdit;
    btnBrowse: TButton;
    btnVerify: TButton;
    dlgOpenExe: TOpenDialog;
    grpSignatures: TGroupBox;
    lvwSignatures: TListView;
    grpSignature: TGroupBox;
    lblDescription: TLabel;
    edtDescription: TEdit;
    lblUrl: TLabel;
    txtUrl: TStaticText;
    lblStatement: TLabel;
    lblClaimedTime: TLabel;
    txtStatement: TStaticText;
    txtClaimedTime: TStaticText;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure lvwSignaturesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure CertManagerPasswordNeeded(Sender: TObject; var Password: string; var Cancel: Boolean);
    procedure lvwSignaturesDeletion(Sender: TObject; Item: TListItem);
  private
    FVerifier: TsbxAuthenticodeVerifier;
    procedure VerifierSignatureFound(Sender: TObject; const IssuerRDN: string;
      const SerialNumber, SubjectKeyID: TBytes; CertFound: Boolean;
      var ValidateSignature, ValidateChain: Boolean);
    procedure VerifierSignatureValidated(Sender: TObject; const IssuerRDN: string;
      const SerialNumber, SubjectKeyID: TBytes; ValidationResult: Integer);
    procedure VerifierTimestampFound(Sender: TObject; const IssuerRDN: string;
      const SerialNumber, SubjectKeyID: TBytes; CertFound: Boolean;
      var ValidateTimestamp, ValidateChain: Boolean);
    procedure VerifierTimestampValidated(Sender: TObject; const IssuerRDN: string;
      const SerialNumber, SubjectKeyID: TBytes; const Time: string;
      ValidationResult, ChainValidationResult, ChainValidationDetails: Integer);
  public
    { Public declarations }
  end;

var
  FormAuthenticodeverifier: TFormAuthenticodeverifier;

implementation

{$R *.dfm}

function CompareArrays(const Arr1, Arr2: TBytes): Boolean;
begin
  if Length(Arr1) = 0 then
    Result := (Length(Arr2) = 0)
  else
  if Length(Arr2) = 0 then
    Result := False
  else
  if Length(Arr1) <> Length(Arr2) then
    Result := False
  else
    Result := CompareMem(@Arr1[0], @Arr2[0], Length(Arr1));
end;

function ValidationResultToStr(Value: TsbxSignatureValidities): string;
begin
  case Value of
    svtValid:
      Result := 'Valid';
    svtCorrupted:
      Result := 'Corrupted';
    svtSignerNotFound:
      Result := 'Signer not found';
    svtFailure:
      Result := 'Failure';
  else
    Result := 'Unknown';
  end;
end;

function StatementTypeToStr(Value: TsbxAuthenticodeStatementTypes): string;
begin
  case Value of
    TsbxAuthenticodeStatementTypes.acsIndividual:
      Result := 'Individual';
    TsbxAuthenticodeStatementTypes.acsCommercial:
      Result := 'Commercial';
  else
    Result := '';
  end;
end;

function TimestampTypeToStr(Value: Integer): string;
begin
  case Value of
    1:
      Result := 'Legacy';
    2:
      Result := 'Trusted';
  else
    Result := '';
  end;
end;

procedure TFormAuthenticodeverifier.lvwSignaturesDeletion(Sender: TObject; Item: TListItem);
var
  Obj: TObject;
begin
  Obj := TObject(Item.Data);
  Item.Data := nil;
  FreeAndNil(Obj);
end;

procedure TFormAuthenticodeverifier.lvwSignaturesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  Signature: TsbxAuthenticodeSignature;
begin
  if (Item = nil) or not Selected then
  begin
    edtDescription.Text := '';
    txtUrl.Caption := '';
    txtStatement.Caption := '';
    txtClaimedTime.Caption := '';
  end
  else
  begin
    Signature := Item.Data;

    edtDescription.Text := Signature.Description;
    txtUrl.Caption := Signature.URL;
    txtStatement.Caption := StatementTypeToStr(Signature.StatementType);
    txtClaimedTime.Caption := Signature.ClaimedSigningTime;
  end;
end;

procedure TFormAuthenticodeverifier.VerifierSignatureFound(Sender: TObject;
  const IssuerRDN: string; const SerialNumber, SubjectKeyID: TBytes;
  CertFound: Boolean; var ValidateSignature, ValidateChain: Boolean);
begin
  ValidateChain := False;
end;

procedure TFormAuthenticodeverifier.VerifierSignatureValidated(Sender: TObject; const IssuerRDN: string;
  const SerialNumber, SubjectKeyID: TBytes; ValidationResult: Integer);
var
  Signature: TsbxAuthenticodeSignature;
  Item: TListItem;
begin
  if FVerifier.Signature = nil then
    Exit;

  Signature := FVerifier.Signature.Clone();

  Item := lvwSignatures.Items.Add();
  Item.Caption := FVerifier.SigningCertificate.Subject;
  Item.SubItems.Add(Signature.HashAlgorithm);
  Item.SubItems.Add(ValidationResultToStr(Signature.SignatureValidationResult));
  Item.Data := Signature;
end;

procedure TFormAuthenticodeverifier.VerifierTimestampFound(Sender: TObject;
  const IssuerRDN: string; const SerialNumber, SubjectKeyID: TArray<System.Byte>;
  CertFound: Boolean; var ValidateTimestamp, ValidateChain: Boolean);
begin
  ValidateChain := False;
end;

procedure TFormAuthenticodeverifier.VerifierTimestampValidated(Sender: TObject; const IssuerRDN: string;
  const SerialNumber, SubjectKeyID: TBytes; const Time: string; ValidationResult, ChainValidationResult,
  ChainValidationDetails: Integer);
var
  Item: TListItem;
begin
  Item := lvwSignatures.Items[lvwSignatures.Items.Count - 1];

  if FVerifier.Timestamp.ValidationResult <> svtValid then
  begin
    Item.SubItems.Add(ValidationResultToStr(FVerifier.Timestamp.ValidationResult));
    Exit;
  end;

  if FVerifier.Signature.ValidatedSigningTime <> '' then
    Item.SubItems.Add(FVerifier.Signature.ValidatedSigningTime)
  else
    Item.SubItems.Add('--');
end;

procedure TFormAuthenticodeverifier.btnBrowseClick(Sender: TObject);
begin
  if edtInputFile.Text <> '' then
    dlgOpenExe.InitialDir := ExtractFilePath(edtInputFile.Text);

  if dlgOpenExe.Execute(Self.Handle) then
  begin
    edtInputFile.Text := dlgOpenExe.FileName;
    lvwSignatures.Items.Clear();
  end;
end;

procedure TFormAuthenticodeverifier.btnVerifyClick(Sender: TObject);
var
  I: Integer;
  Item: TListItem;
  Signature: TsbxAuthenticodeSignature;
begin
  lvwSignaturesSelectItem(lvwSignatures, nil, False);
  lvwSignatures.Items.Clear();

  FVerifier := TsbxAuthenticodeVerifier.Create(nil);
  try
    FVerifier.OnSignatureFound := VerifierSignatureFound;
    FVerifier.OnSignatureValidated := VerifierSignatureValidated;
    FVerifier.OnTimestampFound := VerifierTimestampFound;
    FVerifier.OnTimestampValidated := VerifierTimestampValidated;

    FVerifier.InputFile := edtInputFile.Text;
    try
      FVerifier.Verify();
    except
      on E: Exception do
      begin
        MessageDlg('Failed to verify the file.'#13#10 + E.Message, mtError, [mbOk], 0);
        Exit;
      end;
    end;

    if not FVerifier.Signed then
    begin
      MessageDlg('The file is not singed', mtWarning, [mbOk], 0);
      Exit;
    end;

    if lvwSignatures.Items.Count <> 0 then
      lvwSignatures.Items[0].Selected := True;
  finally
    FreeAndNil(FVerifier);
  end;
end;

procedure TFormAuthenticodeverifier.CertManagerPasswordNeeded(Sender: TObject; var Password: string;
  var Cancel: Boolean);
begin
  Cancel := not InputQuery(Self.Caption, 'Passphrase:', Password);
end;

end.



