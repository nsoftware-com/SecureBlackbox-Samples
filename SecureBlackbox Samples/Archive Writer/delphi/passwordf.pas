unit passwordf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormPassword = class(TForm)
    lblPasswordNeeded: TLabel;
    edtPassword: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure edtPasswordKeyPress(Sender: TObject; var Key: Char);
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

procedure TFormPassword.edtPasswordKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then
    btnOkClick(btnOk);
end;

end.
