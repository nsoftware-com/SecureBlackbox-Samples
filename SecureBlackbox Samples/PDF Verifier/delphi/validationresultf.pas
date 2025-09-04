unit validationresultf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxPDFVerifier;

type
  TFormValidationresult = class(TForm)
    btnClose: TButton;
    lSignatures: TLabel;
    cbSignatures: TComboBox;
    pSignatureInfo: TPanel;
    lAuthorName: TLabel;
    lReason: TLabel;
    lTimestamp: TLabel;
    editAuthorName: TEdit;
    editReason: TEdit;
    btnExtractSigned: TButton;
    lSignatureValidationResult: TLabel;
    lChainValidationResult: TLabel;
    SaveDialog: TSaveDialog;
    procedure cbSignaturesChange(Sender: TObject);
    procedure btnExtractSignedClick(Sender: TObject);
  private
    FVerifier: TsbxPDFVerifier;
  public
    procedure Init(Verifier: TsbxPDFVerifier);
  end;

var
  FormValidationresult: TFormValidationresult;

implementation

{$R *.dfm}

{ TfrmReferences }

procedure TFormValidationresult.Init(Verifier: TsbxPDFVerifier);
var
  i : Integer;
begin
  FVerifier := Verifier;

  cbSignatures.Items.Clear;
  editAuthorName.Text := '';
  editReason.Text := '';
  pSignatureInfo.Visible := false;

  for i := 0 to FVerifier.Signatures.Count - 1 do
    cbSignatures.Items.AddObject(FVerifier.Signatures[i].SignatureName, FVerifier.Signatures[i]);
end;

procedure TFormValidationresult.btnExtractSignedClick(Sender: TObject);
begin
  if cbSignatures.ItemIndex <> -1 then
    if SaveDialog.Execute then
    begin
      FVerifier.OutputFile := SaveDialog.Filename;
      FVerifier.GetSignedVersion(cbSignatures.Text);
    end;
end;

procedure TFormValidationresult.cbSignaturesChange(Sender: TObject);
var
  Sig : TsbxPDFSignature;
begin
  if cbSignatures.ItemIndex <> -1 then
  begin
    Sig := TsbxPDFSignature(cbSignatures.Items.Objects[cbSignatures.ItemIndex]);
    if Sig.AuthorName <> '' then
      editAuthorName.Text := Sig.AuthorName
    else
      editAuthorName.Text := '<not specified>';
    if Sig.Reason <> '' then
      editReason.Text := Sig.Reason
    else
      editReason.Text := '<not specified>';
    lTimestamp.Caption := 'Timestamp: ' + Sig.ClaimedSigningTime + ' (local)';
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