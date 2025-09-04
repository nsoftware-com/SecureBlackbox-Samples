unit changekeyf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormChangeKey = class(TForm)
    Label4: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    edOldPassword: TEdit;
    Label1: TLabel;
    edNewPassword: TEdit;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormChangeKey: TFormChangeKey;

implementation

{$R *.dfm}

procedure TFormChangeKey.btnOKClick(Sender: TObject);
begin
  if edNewPassword.Text = '' then
    MessageDlg('Please set new password', mtError, [mbOk], 0)
  else
    ModalResult := mrOk;
end;

procedure TFormChangeKey.FormShow(Sender: TObject);
begin
  edNewPassword.Text := '';
  edOldPassword.SetFocus;
end;

end.
