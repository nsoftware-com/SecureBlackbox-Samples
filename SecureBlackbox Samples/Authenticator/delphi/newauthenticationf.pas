unit newauthenticationf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormNewAuthentication = class(TForm)
    Label1: TLabel;
    eUserId: TEdit;
    Label2: TLabel;
    eDefAuthMethods: TEdit;
    bBaseMethods: TButton;
    bStart: TButton;
    procedure bBaseMethodsClick(Sender: TObject);
    procedure bStartClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormNewAuthentication: TFormNewAuthentication;

implementation

{$R *.dfm}

procedure TFormNewAuthentication.bBaseMethodsClick(Sender: TObject);
begin
  eDefAuthMethods.Text := 'password;otp-h;otp-t;dc';
end;

procedure TFormNewAuthentication.bStartClick(Sender: TObject);
begin
  if eUserId.Text = '' then
    MessageDlg('Please enter user id', mtError, [mbOk], 0)
  else
    ModalResult := mrOk;
end;

end.
