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
    Verifier: TsbxAuthenticodeVerifier;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure lvwSignaturesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure CertManagerPasswordNeeded(Sender: TObject; var Password: string; var Cancel: Boolean);
    procedure VerifierSignatureFound(Sender: TObject; const IssuerRDN: string;
      SerialNumber, SubjectKeyID: TArray<System.Byte>; CertFound: Boolean;
      var ValidateSignature, ValidateChain: Boolean);
    procedure VerifierTimestampFound(Sender: TObject; const IssuerRDN: string;
      SerialNumber, SubjectKeyID: TArray<System.Byte>; CertFound: Boolean;
      var ValidateTimestamp, ValidateChain: Boolean);
  private
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
  const IssuerRDN: string; SerialNumber, SubjectKeyID: TArray<System.Byte>;
  CertFound: Boolean; var ValidateSignature, ValidateChain: Boolean);
begin
  ValidateChain := false;
end;

procedure TFormAuthenticodeverifier.VerifierTimestampFound(Sender: TObject;
  const IssuerRDN: string; SerialNumber, SubjectKeyID: TArray<System.Byte>;
  CertFound: Boolean; var ValidateTimestamp, ValidateChain: Boolean);
begin
  ValidateChain := false;
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
  Verifier.InputFile := edtInputFile.Text;
  try
    Verifier.Verify();
  except
    on E: Exception do
    begin
      MessageDlg('Failed to verify the file.'#13#10 + E.Message, mtError, [mbOk], 0);
      Exit;
    end;
  end;

  if not Verifier.Signed then
  begin
    MessageDlg('The file is not singed', mtWarning, [mbOk], 0);
    Exit;
  end;

  for I := 0 to Verifier.Signatures.Count - 1 do
  begin
    Signature := Verifier.Signatures[I];
    Item := lvwSignatures.Items.Add();
    Item.Caption := IntToStr(I);
    Item.SubItems.Add(Signature.HashAlgorithm);
    Item.SubItems.Add(ValidationResultToStr(Signature.SignatureValidationResult));
    if Signature.ValidatedSigningTime <> '' then
      Item.SubItems.Add(Signature.ValidatedSigningTime)
    else
      Item.SubItems.Add('--');
    Item.Data := Signature;
  end;

  if lvwSignatures.Items.Count <> 0 then
    lvwSignatures.Items[0].Selected := True;
end;

procedure TFormAuthenticodeverifier.CertManagerPasswordNeeded(Sender: TObject; var Password: string;
  var Cancel: Boolean);
begin
  Cancel := not InputQuery(Self.Caption, 'Passphrase:', Password);
end;

end.



