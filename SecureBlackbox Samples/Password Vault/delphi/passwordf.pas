unit passwordf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormPassword = class(TForm)
    lblPasswordNeeded: TLabel;
    edPassword: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure edPasswordKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormPassword: TFormPassword;

implementation

{$R *.dfm}

procedure TFormPassword.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormPassword.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormPassword.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  Resize := false;
end;

procedure TFormPassword.FormShow(Sender: TObject);
begin
  edPassword.Text := '';
  edPassword.SetFocus;
end;

procedure TFormPassword.edPasswordKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then
    btnOkClick(btnOk);
end;

end.
