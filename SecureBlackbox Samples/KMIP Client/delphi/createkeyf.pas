unit createkeyf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormCreateKey = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblSelectPublicKeyLen: TLabel;
    lblCurve: TLabel;
    edGroup: TEdit;
    cbPublicAlgorithm: TComboBox;
    cbCurve: TComboBox;
    edKeyLength: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure cbPublicAlgorithmChange(Sender: TObject);
  private
    { Private declarations }
    FCurve: string;
  public
    { Public declarations }
    property Curve: string read FCurve;
  end;

var
  FormCreateKey: TFormCreateKey;

implementation

{$R *.dfm}

procedure TFormCreateKey.btnOkClick(Sender: TObject);
begin
  if cbPublicAlgorithm.ItemIndex = 2 then
    FCurve := cbCurve.Text
  else
    FCurve := '';

  ModalResult := mrOk;
end;

procedure TFormCreateKey.cbPublicAlgorithmChange(Sender: TObject);
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

  case cbPublicAlgorithm.ItemIndex of
    3: edKeyLength.Text := '64';
    4: edKeyLength.Text := '192';
    5: edKeyLength.Text := '256';
    else
      edKeyLength.Text := '1024';
  end;
end;

end.
