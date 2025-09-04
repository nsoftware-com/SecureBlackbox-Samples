unit validationresultf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxOfficeVerifier;

type
  TFormValidationresult = class(TForm)
    btnClose: TButton;
    lSignatures: TLabel;
    cbSignatures: TComboBox;
    pSignatureInfo: TPanel;
    lSignatureType: TLabel;
    editSignatureType: TEdit;
    lSignatureValidationResult: TLabel;
    lChainValidationResult: TLabel;
    lIsDocumentSigned: TLabel;
    lIsSignatureOriginSigned: TLabel;
    lIsCorePropertiesSigned: TLabel;
    lSignatureTime: TLabel;
    procedure cbSignaturesChange(Sender: TObject);
  private
    FVerifier: TsbxOfficeVerifier;
  public
    procedure Init(Verifier: TsbxOfficeVerifier);
  end;

var
  FormValidationresult: TFormValidationresult;

implementation

{$R *.dfm}

{ TfrmReferences }

procedure TFormValidationresult.Init(Verifier: TsbxOfficeVerifier);
var
  i: integer;
begin
  FVerifier := Verifier;

  cbSignatures.Items.Clear;
  editSignatureType.Text := '';
  pSignatureInfo.Visible := false;
  for I := 0 to FVerifier.Signatures.Count - 1 do
    cbSignatures.Items.AddObject('Signature ' + IntToStr(i + 1),
      FVerifier.Signatures.Item[i]);
end;

procedure TFormValidationresult.cbSignaturesChange(Sender: TObject);
var
  sig : TsbxOfficeSignature;
begin
  if cbSignatures.ItemIndex <> -1 then
  begin
    sig := TsbxOfficeSignature(cbSignatures.Items.Objects[cbSignatures.ItemIndex]);

    case sig.SignatureType of
      ostBinaryCryptoAPI: editSignatureType.Text := 'BinaryCryptoAPI';
      ostBinaryXML: editSignatureType.Text := 'BinaryXML';
      ostOpenXML: editSignatureType.Text := 'OpenXML';
      ostOpenXPS: editSignatureType.Text := 'OpenXPS';
      ostOpenDocument : editSignatureType.Text := 'OpenOffice';
      else
        editSignatureType.Text := 'Unknown';
    end;

    if sig.DocumentSigned then
      lIsDocumentSigned.Caption := 'Document content is signed'
    else
      lIsDocumentSigned.Caption := 'Document content is partially signed';

    if sig.CorePropertiesSigned then
      lIsCorePropertiesSigned.Caption := 'Document properties are signed'
    else
      lIsCorePropertiesSigned.Caption := 'Document properties are not signed';

    if sig.SignatureOriginSigned then
      lIsSignatureOriginSigned.Caption := 'Signature origin is signed'
    else
      lIsSignatureOriginSigned.Caption := 'Signature origin is not signed';

    lSignatureTime.Caption := 'Signature Time: ' + sig.ClaimedSigningTime;

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
      cvtValidButUntrusted: lChainValidationResult.Caption := 'Chain Validation Result: ValidButUntrusted';
      cvtInvalid: lChainValidationResult.Caption := 'Chain Validation Result: Invalid';
      cvtCantBeEstablished: lChainValidationResult.Caption := 'Chain Validation Result: CantBeEstablished';
    end;

    pSignatureInfo.Visible := true;
  end
  else
    pSignatureInfo.Visible := false;
end;

end.
