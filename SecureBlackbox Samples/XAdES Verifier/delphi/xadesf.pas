unit xadesf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxXAdESVerifier;

type
  TFormXades = class(TForm)
    btnOK: TButton;
    lbSignedTime: TLabel;
    lbTimestamp: TLabel;
    lbTimestampSerial: TLabel;
    lbVersion: TLabel;
    Label2: TLabel;
    edVersion: TEdit;
    edForm: TEdit;
  private
  public
    procedure Initialize(Verifier: TsbxXAdESVerifier; SigIndex : Integer);
  end;

var
  FormXades: TFormXades;

implementation

{$R *.DFM}

procedure TFormXades.Initialize(Verifier: TsbxXAdESVerifier; SigIndex : Integer);
var
  ProductionPlace: string;
begin
  case Verifier.Signatures[SigIndex].XAdESVersion of
    TsbxXAdESVersions.xav111: edVersion.Text := '1.1.1';
    TsbxXAdESVersions.xav122: edVersion.Text := '1.2.2';
    TsbxXAdESVersions.xav132: edVersion.Text := '1.3.2';
    TsbxXAdESVersions.xav141: edVersion.Text := '1.4.1';
  else
    edVersion.Text := 'Unknown';
  end;

  case Verifier.Signatures[SigIndex].Level of
    TsbxAdESSignatureLevels.aslGeneric: edForm.Text := 'XML-DSIG';
    TsbxAdESSignatureLevels.aslBaselineB: edForm.Text := 'XAdES Baseline B';
    TsbxAdESSignatureLevels.aslBaselineT: edForm.Text := 'XAdES Baseline T';
    TsbxAdESSignatureLevels.aslBaselineLT: edForm.Text := 'XAdES Baseline LT';
    TsbxAdESSignatureLevels.aslBaselineLTA: edForm.Text := 'XAdES Baseline LTA';
    TsbxAdESSignatureLevels.aslBES: edForm.Text := 'XAdES-BES';
    TsbxAdESSignatureLevels.aslEPES: edForm.Text := 'XAdES-EPES';
    TsbxAdESSignatureLevels.aslT: edForm.Text := 'XAdES-T';
    TsbxAdESSignatureLevels.aslC: edForm.Text := 'XAdES-C';
    TsbxAdESSignatureLevels.aslX: edForm.Text := 'XAdES-X';
    TsbxAdESSignatureLevels.aslXL: edForm.Text := 'XAdES-X-L';
    TsbxAdESSignatureLevels.aslA: edForm.Text := 'XAdES-A';
    TsbxAdESSignatureLevels.aslExtendedBES: edForm.Text := 'XAdES-E-BES';
    TsbxAdESSignatureLevels.aslExtendedEPES: edForm.Text := 'XAdES-E-EPES';
    TsbxAdESSignatureLevels.aslExtendedT: edForm.Text := 'XAdES-E-T';
    TsbxAdESSignatureLevels.aslExtendedC: edForm.Text := 'XAdES-E-C';
    TsbxAdESSignatureLevels.aslExtendedX: edForm.Text := 'XAdES-E-X';
    TsbxAdESSignatureLevels.aslExtendedXLong: edForm.Text := 'XAdES-E-X-Long';
    TsbxAdESSignatureLevels.aslExtendedXL: edForm.Text := 'XAdES-E-X-L';
    TsbxAdESSignatureLevels.aslExtendedA: edForm.Text := 'XAdES-E-A';
    else
      edForm.Text := 'Unknown';
  end;

  if Verifier.Signatures[SigIndex].ClaimedSigningTime <> '' then
    lbSignedTime.Caption := 'Signed Time: ' + Verifier.Signatures[SigIndex].ClaimedSigningTime + ' UTC'
  else
    lbSignedTime.Caption := '';
end;

end.