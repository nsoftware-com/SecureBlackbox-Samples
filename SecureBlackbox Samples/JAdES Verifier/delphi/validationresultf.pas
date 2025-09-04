unit validationresultf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxJAdESVerifier;

type
  TFormValidationresult = class(TForm)
    btnClose: TButton;
    lSignatures: TLabel;
    cbSignatures: TComboBox;
    pSignatureInfo: TPanel;
    lTimestamp: TLabel;
    lSignatureValidationResult: TLabel;
    lChainValidationResult: TLabel;
    lTimestamped: TLabel;
    procedure cbSignaturesChange(Sender: TObject);
  private
    FVerifier: TsbxJAdESVerifier;
  public
    procedure Init(Verifier: TsbxJAdESVerifier);
  end;

var
  FormValidationresult: TFormValidationresult;

implementation

{$R *.dfm}

{ TfrmReferences }

procedure TFormValidationresult.Init(Verifier: TsbxJAdESVerifier);
var
  I : Integer;
begin
  FVerifier := Verifier;

  cbSignatures.Items.Clear;
  pSignatureInfo.Visible := false;

  for I := 0 to FVerifier.Signatures.Count - 1 do
    cbSignatures.Items.AddObject('Signature #' + IntToStr(I + 1), FVerifier.Signatures[I]);

  cbSignatures.ItemIndex := 0;
  cbSignaturesChange(cbSignatures);
end;

procedure TFormValidationresult.cbSignaturesChange(Sender: TObject);
var
  Sig : TsbxJAdESSignature;
  s : string;
begin
  if cbSignatures.ItemIndex <> -1 then
  begin
    Sig := TsbxJAdESSignature(cbSignatures.Items.Objects[cbSignatures.ItemIndex]);

    lTimestamped.Caption := 'Timestamped: ' + BoolToStr(Sig.Timestamped, true);
    if Sig.ValidatedSigningTime <> '' then
      lTimestamp.Caption := 'Validated Signing Time: ' + Sig.ValidatedSigningTime + ' UTC'
    else
    if Sig.ClaimedSigningTime <> '' then
      lTimestamp.Caption := 'Claimed Signing Time: ' + Sig.ClaimedSigningTime + ' UTC'
    else
      lTimestamp.Caption := '';

    case sig.SignatureValidationResult of
      svtValid: s := 'Valid';
      svtCorrupted: s := 'Corrupted';
      svtSignerNotFound: s := 'SignerNotFound';
      svtFailure: s := 'Failure';
      else
        s := 'Unknown';
    end;

    lSignatureValidationResult.Caption := 'Signature Validation Result: ' + s;

    case sig.ChainValidationResult of
      cvtValid: s := 'Valid';
      cvtValidButUntrusted: s := 'ValidButUntrusted';
      cvtInvalid: s := 'Invalid';
      cvtCantBeEstablished: s := 'CantBeEstablished';
      else
        s := 'Unknown';
    end;

    lChainValidationResult.Caption := 'Chain Validation Result: ' + s;

    pSignatureInfo.Visible := true;
  end
  else
    pSignatureInfo.Visible := false;
end;

end.