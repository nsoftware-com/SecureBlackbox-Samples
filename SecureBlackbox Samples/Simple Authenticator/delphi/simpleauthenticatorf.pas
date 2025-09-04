(*
 * SecureBlackbox 2024 Delphi Edition - Sample Project
 *
 * This sample project demonstrates the usage of SecureBlackbox in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/secureblackbox
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 *)
unit simpleauthenticatorf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxUserManager, SBxAuthenticator;

(*
Users:
      Name    Password        HOTP

      user    password

      admin   adminpassword   1862287066
*)

type
  TFormSimpleAuthenticator = class(TForm)
    Label1: TLabel;
    eUserId: TEdit;
    bStart: TButton;
    Label10: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bStartClick(Sender: TObject);
  private
    { Private declarations }
    FAuthenticator: TsbxAuthenticator;

    function ContinueAuthentication: integer;
  public
    { Public declarations }
  end;

var
  FormSimpleAuthenticator: TFormSimpleAuthenticator;

implementation

{$R *.dfm}

uses continueauthenticationf;

function TFormSimpleAuthenticator.ContinueAuthentication: integer;
var
  ContinueAuthenticationForm: TFormContinueAuthentication;
begin
  ContinueAuthenticationForm := TFormContinueAuthentication.Create(nil);
  try
    ContinueAuthenticationForm.eAuthMethod.Text := FAuthenticator.AuthInfo.AuthMethod;

    if ContinueAuthenticationForm.ShowModal = mrOk then
      Result := FAuthenticator.ContinueAuth(FAuthenticator.AuthInfo.State, ContinueAuthenticationForm.eAuthToken.Text)
    else
      Result := 3;
  finally
    FreeAndNil(ContinueAuthenticationForm);
  end;
end;

procedure TFormSimpleAuthenticator.bStartClick(Sender: TObject);
var
  i, Res: integer;
  UserFind: boolean;
begin
  // Check user in Users
  UserFind := false;
  for i := 0 to FAuthenticator.Users.Count - 1 do
    if FAuthenticator.Users.Item[i].Username = eUserId.Text then
    begin
      UserFind := true;
      break;
    end;

  if not UserFind then
  begin
    MessageDlg('Authentication failed. User not found', mtInformation, [mbOk], 0);
    exit;
  end;

  // Start authentication
  Res := FAuthenticator.StartAuth(eUserId.Text);

  while Res = 0 do // Further Auth Needed
  begin
    Res := ContinueAuthentication;
  end;

  if Res = 1 then // Succeeded
    MessageDlg('Authentication succeeded', mtInformation, [mbOk], 0)
  else
  if Res = 2 then // Failed
    MessageDlg('Authentication failed', mtInformation, [mbOk], 0)
  else
    MessageDlg('Canceled by user', mtInformation, [mbOk], 0);
end;

procedure TFormSimpleAuthenticator.FormCreate(Sender: TObject);
var
  UserManager: TsbxUserManager;
begin
  FAuthenticator := TsbxAuthenticator.Create(nil);

  UserManager := TsbxUserManager.Create(nil);
  try
    UserManager.ImportFromFile('users.usr', 'password', true);

    FAuthenticator.Users := UserManager.Users;
  finally
    FreeAndNil(UserManager);
  end;
end;

procedure TFormSimpleAuthenticator.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FAuthenticator);
end;

end.


