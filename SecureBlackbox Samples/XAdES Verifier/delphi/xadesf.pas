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

  case Verifier.Signatures[SigIndex].XAdESForm of
    TsbxXAdESForms.xafBasic: edForm.Text := 'XAdES';
    TsbxXAdESForms.xafBES: edForm.Text := 'XAdES-BES';
    TsbxXAdESForms.xafEPES: edForm.Text := 'XAdES-EPES';
    TsbxXAdESForms.xafT: edForm.Text := 'XAdES-T';
    TsbxXAdESForms.xafC: edForm.Text := 'XAdES-C';
    TsbxXAdESForms.xafX: edForm.Text := 'XAdES-X';
    TsbxXAdESForms.xafXL: edForm.Text := 'XAdES-X-L';
    TsbxXAdESForms.xafA: edForm.Text := 'XAdES-A';
    TsbxXAdESForms.xafExtendedBES: edForm.Text := 'XAdES-E-BES';
    TsbxXAdESForms.xafExtendedEPES: edForm.Text := 'XAdES-E-EPES';
    TsbxXAdESForms.xafExtendedT: edForm.Text := 'XAdES-E-T';
    TsbxXAdESForms.xafExtendedC: edForm.Text := 'XAdES-E-C';
    TsbxXAdESForms.xafExtendedX: edForm.Text := 'XAdES-E-X';
    TsbxXAdESForms.xafExtendedXLong: edForm.Text := 'XAdES-E-X-Long';
    TsbxXAdESForms.xafExtendedXL: edForm.Text := 'XAdES-E-X-L';
    TsbxXAdESForms.xafExtendedA: edForm.Text := 'XAdES-E-A';
    else
      edForm.Text := 'Unknown';
  end;

  if Verifier.Signatures[SigIndex].ClaimedSigningTime <> '' then
    lbSignedTime.Caption := 'Signed Time: ' + Verifier.Signatures[SigIndex].ClaimedSigningTime + ' UTC'
  else
    lbSignedTime.Caption := '';
end;

end.