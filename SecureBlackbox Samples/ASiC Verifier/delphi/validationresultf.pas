unit validationresultf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxASiCVerifier;

type
  TFormValidationresult = class(TForm)
    btnClose: TButton;
    lSignatures: TLabel;
    cbSignatures: TComboBox;
    pSignatureInfo: TPanel;
    lSignatureValidationResult: TLabel;
    lSignatureName: TLabel;
    lIssuerRDN: TLabel;
    lSerialNumber: TLabel;
    lSubjectKeyID: TLabel;
    edIssuerRDN: TEdit;
    edSignatureType: TEdit;
    edSerialNumber: TEdit;
    edSubjectKeyID: TEdit;
    edTimestamp: TEdit;
    lTimestamp: TLabel;
    lSignedFiles: TLabel;
    edSignedFiles: TEdit;
    Label1: TLabel;
    edLevel: TEdit;
    lChainValidationResult: TLabel;
    procedure cbSignaturesChange(Sender: TObject);
  private
    FVerifier: TsbxASiCVerifier;
  public
    procedure Init(Verifier: TsbxASiCVerifier);
  end;

var
  FormValidationresult: TFormValidationresult;

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

procedure TFormValidationresult.Init(Verifier: TsbxASiCVerifier);
var
  i: integer;
begin
  FVerifier := Verifier;

  cbSignatures.Items.Clear;
  pSignatureInfo.Visible := false;
  for I := 0 to FVerifier.Signatures.Count - 1 do
    cbSignatures.Items.AddObject('Signature#' + IntToStr(i + 1),
      FVerifier.Signatures.Item[i]);
end;

procedure TFormValidationresult.cbSignaturesChange(Sender: TObject);
var
  Sig : TsbxASiCSignature;
begin
  if cbSignatures.ItemIndex <> -1 then
  begin
    Sig := TsbxASiCSignature(cbSignatures.Items.Objects[cbSignatures.ItemIndex]);

    case Sig.Level of
      aslBES: edLevel.Text := 'BES';
      aslEPES: edLevel.Text := 'EPES';
      aslT: edLevel.Text := 'T';
      aslC: edLevel.Text := 'C';
      aslXType1: edLevel.Text := 'XType1';
      aslXType2: edLevel.Text := 'XType2';
      aslXLType1: edLevel.Text := 'XLType1';
      aslXLType2: edLevel.Text := 'XLType2';
      aslBaselineB: edLevel.Text := 'Baseline B';
      aslBaselineT: edLevel.Text := 'Baseline T';
      aslBaselineLT: edLevel.Text := 'Baseline LT';
      aslBaselineLTA: edLevel.Text := 'Baseline LTA';
      aslExtendedBES: edLevel.Text := 'Extended BES';
      aslExtendedEPES: edLevel.Text := 'Extended EPES';
      aslExtendedT: edLevel.Text := 'Extended T';
      aslExtendedC: edLevel.Text := 'Extended C';
      aslExtendedXType1: edLevel.Text := 'Extended XType1';
      aslExtendedXType2: edLevel.Text := 'Extended XType2';
      aslExtendedXLType1: edLevel.Text := 'Extended XLType1';
      aslExtendedXLType2: edLevel.Text := 'Extended XLType2';
      aslA: edLevel.Text := 'A';
      aslExtendedA: edLevel.Text := 'Extended A';
      else
        edLevel.Text := 'Unknown';
    end;

    case Sig.SignatureType of
      castCAdES: edSignatureType.Text := 'CAdES';
      castXAdES: edSignatureType.Text := 'XAdES';
      castTimestamp: edSignatureType.Text := 'Timestamp';
      else
        edSignatureType.Text := 'Unknown';
    end;

    edIssuerRDN.Text := Sig.IssuerRDN;
    edSerialNumber.Text := BinaryToString(Sig.SerialNumber);
    edSubjectKeyID.Text := BinaryToString(Sig.SubjectKeyID);

    if Sig.SignatureType = castTimestamp then
      edTimestamp.Text := Sig.ClaimedSigningTime
    else
      edTimestamp.Text := '';

    edSignedFiles.Text := Sig.SignedFiles;

    case sig.SignatureValidationResult of
      svtValid: lSignatureValidationResult.Caption := 'Signature Validation Result: Valid';
      svtCorrupted: lSignatureValidationResult.Caption := 'Signature Validation Result: Corrupted';
      svtSignerNotFound: lSignatureValidationResult.Caption := 'Signature Validation Result: SignerNotFound';
      svtFailure: lSignatureValidationResult.Caption := 'Signature Validation Result: Failure';
      else
        lSignatureValidationResult.Caption := 'Signature Validation Result: Unknown';
    end;

    case sig.ChainValidationResult of
      cvtValid: lChainValidationResult.Caption := 'Chain Validation Result: Valid';
      cvtValidButUntrusted: lChainValidationResult.Caption := 'Chain Validation Result: Valid but untrusted';
      cvtInvalid: lChainValidationResult.Caption := 'Chain Validation Result: Invalid';
      cvtCantBeEstablished: lChainValidationResult.Caption := 'Chain Validation Result: Can''t be established';
      else
        lChainValidationResult.Caption := 'Chain Validation Result: Unknown';
    end;

    pSignatureInfo.Visible := true;
  end
  else
    pSignatureInfo.Visible := false;
end;

end.