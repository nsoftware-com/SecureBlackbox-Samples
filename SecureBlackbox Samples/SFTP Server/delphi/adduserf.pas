unit adduserf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Buttons;

type
  TFormadduser = class(TForm)
    edName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edPassword: TEdit;
    Button1: TButton;
    Button2: TButton;
    lbPrivateKey: TLabel;
    edPublicKey: TEdit;
    sbPublicKey: TSpeedButton;
    OpenDialog: TOpenDialog;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure sbPublicKeyClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Formadduser: TFormadduser;

implementation

{$R *.dfm}

procedure TFormadduser.Button1Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormadduser.Button2Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormadduser.sbPublicKeyClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    edPublicKey.Text := OpenDialog.Filename;
end;

end.
