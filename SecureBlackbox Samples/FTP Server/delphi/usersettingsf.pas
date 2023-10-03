unit usersettingsf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SBxTypes;

type
  TFormUsersettings = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    editUsername: TEdit;
    lUser: TLabel;
    lPassword: TLabel;
    editPassword: TEdit;
    lHomeDirectory: TLabel;
    editHomeDirectory: TEdit;
    lSpeedLimit: TLabel;
    editSpeedLimit: TEdit;
    lKBSec: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FUser: TsbxUserAccount;
  public
    procedure Initialize(User: TsbxUserAccount);
  end;

var
  FormUsersettings: TFormUsersettings;

implementation

{$R *.DFM}

uses
  SetPasswordF, SetPublicKeyF;

procedure TFormUsersettings.btnOKClick(Sender: TObject);
var
  SpeedLimit: integer;
begin
  if FUser <> nil then
  begin
    FUser.BasePath := editHomeDirectory.Text;
    if editPassword.Text <> '********' then
      FUser.Password := editPassword.Text;
    if not TryStrToInt(editSpeedLimit.Text, SpeedLimit) then
      SpeedLimit := 0;

    FUser.IncomingSpeedLimit := SpeedLimit;
    FUser.OutgoingSpeedLimit := SpeedLimit;
  end;

  ModalResult := mrOk;
end;

procedure TFormUsersettings.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormUsersettings.Initialize(User: TsbxUserAccount);
begin
  FUser := User;
  if User <> nil then
  begin
    editUsername.Text := User.UserName;
    editUsername.Enabled := false;
    editPassword.Text := '********';
    editHomeDirectory.Text := User.BasePath;
    editSpeedLimit.Text := IntToStr(User.OutgoingSpeedLimit);
  end
  else
  begin
    editUsername.Text := '';
    editUsername.Enabled := true;
    editPassword.Text := '';
    editHomeDirectory.Text := '';
    editSpeedLimit.Text := '0';
  end;
end;

end.
