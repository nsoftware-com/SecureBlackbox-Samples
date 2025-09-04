unit setpasswordf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormSetpassword = class(TForm)
    gbSetPassword: TGroupBox;
    lPass: TLabel;
    editPassword: TEdit;
    lPassConf: TLabel;
    editPasswordConf: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSetpassword: TFormSetpassword;

implementation

{$R *.DFM}

procedure TFormSetpassword.btnOKClick(Sender: TObject);
begin
  if editPassword.Text = editPasswordConf.Text then
    ModalResult := mrOk
  else
    MessageDlg('Confirmation does not match password', mtError, [mbOk], 0);
end;

procedure TFormSetpassword.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormSetpassword.FormShow(Sender: TObject);
begin
  editPassword.Text := '';
  editPasswordConf.Text := '';
end;

end.
