unit createcertf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormCreateCert = class(TForm)
    GroupBox1: TGroupBox;
    edGroup: TEdit;
    Label1: TLabel;
    cbPublicAlgorithm: TComboBox;
    Label2: TLabel;
    cbHashAlgorithm: TComboBox;
    Label3: TLabel;
    lblSelectPublicKeyLen: TLabel;
    lblCurve: TLabel;
    gbSubject: TGroupBox;
    lblCountry: TLabel;
    lblState: TLabel;
    lblLocality: TLabel;
    lblOrganization: TLabel;
    lblOrganizationUnit: TLabel;
    lblCommonName: TLabel;
    edStateS: TEdit;
    edLocalityS: TEdit;
    edOrganizationS: TEdit;
    edOrganizationUnitS: TEdit;
    edCommonNameS: TEdit;
    cbCountryS: TComboBox;
    btnOk: TButton;
    btnCancel: TButton;
    cbCurve: TComboBox;
    edKeyLength: TEdit;
    procedure cbPublicAlgorithmChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
    FCurve: string;
  public
    { Public declarations }
    property Curve: string read FCurve;
  end;

var
  FormCreateCert: TFormCreateCert;

implementation

{$R *.dfm}

uses CountryList;

procedure TFormCreateCert.btnOkClick(Sender: TObject);
begin
  if (cbCountryS.ItemIndex < 0) or (edLocalityS.Text = '') or (edOrganizationS.Text = '') or (edCommonNameS.Text = '') then
  begin
    ShowMessage('One or several subject fields are empty. Correct, please.');
    Exit;
  end;

  if cbPublicAlgorithm.ItemIndex = 2 then
    FCurve := cbCurve.Text
  else
    FCurve := '';

  ModalResult := mrOk;
end;

procedure TFormCreateCert.cbPublicAlgorithmChange(Sender: TObject);
begin
  if cbPublicAlgorithm.ItemIndex = 2 then
  begin
    lblCurve.Enabled := true;
    cbCurve.Enabled := true;
  end
  else
  begin
    lblCurve.Enabled := false;
    cbCurve.Enabled := false;
  end;
end;

procedure TFormCreateCert.FormCreate(Sender: TObject);
begin
  FillCountryCombo(cbCountryS);
end;

end.
