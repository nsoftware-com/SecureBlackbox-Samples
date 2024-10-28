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
unit authenticatorf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxUserManager, SBxAuthenticator;

type
  TFormAuthenticator = class(TForm)
    Label10: TLabel;
    bNew: TButton;
    bContinue: TButton;
    Label1: TLabel;
    eUsersFile: TEdit;
    bUsersFile: TButton;
    dlgOpen: TOpenDialog;
    Label2: TLabel;
    ePassword: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bUsersFileClick(Sender: TObject);
    procedure bNewClick(Sender: TObject);
    procedure bContinueClick(Sender: TObject);
  private
    { Private declarations }
    FAuthenticator: TsbxAuthenticator;
    FChangeAuthMethodsResult: boolean;

    function StartAuthentication: integer;
    function ContinueAuthentication: integer;

    procedure DoAuthStart(Sender: TObject; const UserID: String; var AuthMethods: String);
    procedure DoAuthAttemptStart(Sender: TObject; const UserID: String;
      const AuthMethod: String; const RemainingAuthMethods: String);
    procedure DoCustomAuthStart(Sender: TObject; const UserID: String;
      const AuthMethod: String; var AuthMethodPars: String; var AuthMethodData: String);
    procedure DoAuthVerify(Sender: TObject; const UserID: String;
      const AuthMethod: String; const AuthToken: String;
      const AuthMethodData: String; var Valid: Boolean);
    procedure DoAuthAttemptResult(Sender: TObject; const UserID: String;
      const AuthMethod: String; var AuthRes: Integer; var RemainingAuthMethods: String);
  public
    { Public declarations }
  end;

var
  FormAuthenticator: TFormAuthenticator;

implementation

{$R *.dfm}

uses continueauthenticationf, newauthenticationf, changeauthmethodsf,
  dcsettingsf, customauthstartf, changeauthresultf, authverifyf;

function TFormAuthenticator.StartAuthentication: integer;
var
  NewAuthenticationForm: TFormNewAuthentication;
begin
  NewAuthenticationForm := TFormNewAuthentication.Create(nil);
  try
    if NewAuthenticationForm.ShowModal = mrOk then
    begin
      FAuthenticator.DefaultAuthMethods := NewAuthenticationForm.eDefAuthMethods.Text;

      Result := FAuthenticator.StartAuth(NewAuthenticationForm.eUserId.Text);
    end
    else
      Result := 3;
  finally
    FreeAndNil(NewAuthenticationForm);
  end;
end;

function TFormAuthenticator.ContinueAuthentication(): integer;
var
  ContinueAuthenticationForm: TFormContinueAuthentication;
begin
  ContinueAuthenticationForm := TFormContinueAuthentication.Create(nil);
  try
    ContinueAuthenticationForm.eAuthState.Text := FAuthenticator.AuthInfo.State;
    ContinueAuthenticationForm.eAuthMethod.Text := FAuthenticator.AuthInfo.AuthMethod;

    if ContinueAuthenticationForm.ShowModal = mrOk then
    begin
      FChangeAuthMethodsResult := ContinueAuthenticationForm.cbAuthMethodsResult.Checked;

      Result := FAuthenticator.ContinueAuth(ContinueAuthenticationForm.eAuthState.Text, ContinueAuthenticationForm.eAuthToken.Text);
    end
    else
      Result := 3;
  finally
    FreeAndNil(ContinueAuthenticationForm);
  end;
end;

procedure TFormAuthenticator.bNewClick(Sender: TObject);
var
  Res: integer;
  UserManager: TsbxUserManager;
begin
  if (eUsersFile.Text <> '') and FileExists(eUsersFile.Text) then
  begin
    UserManager := TsbxUserManager.Create(nil);
    try
      UserManager.ImportFromFile(eUsersFile.Text, ePassword.Text, true);

      FAuthenticator.Users := UserManager.Users;
    finally
      FreeAndNil(UserManager);
    end;
  end;

  Res := StartAuthentication;

  if Res = 3 then // Canceled by user
    exit;

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

procedure TFormAuthenticator.bContinueClick(Sender: TObject);
var
  Res: integer;
  UserManager: TsbxUserManager;
begin
  if (eUsersFile.Text <> '') and FileExists(eUsersFile.Text) then
  begin
    UserManager := TsbxUserManager.Create(nil);
    try
      UserManager.ImportFromFile(eUsersFile.Text, ePassword.Text, true);

      FAuthenticator.Users := UserManager.Users;
    finally
      FreeAndNil(UserManager);
    end;
  end;

  Res := ContinueAuthentication;

  if Res = 3 then // Canceled by user
    exit;

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

procedure TFormAuthenticator.bUsersFileClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    eUsersFile.Text := dlgOpen.FileName;
end;

procedure TFormAuthenticator.FormCreate(Sender: TObject);
begin
  FAuthenticator := TsbxAuthenticator.Create(nil);
  FAuthenticator.OnAuthStart := DoAuthStart;
  FAuthenticator.OnAuthAttemptStart := DoAuthAttemptStart;
  FAuthenticator.OnCustomAuthStart := DoCustomAuthStart;
  FAuthenticator.OnAuthVerify := DoAuthVerify;
  FAuthenticator.OnAuthAttemptResult := DoAuthAttemptResult;
end;

procedure TFormAuthenticator.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FAuthenticator);
end;


procedure TFormAuthenticator.DoAuthStart(Sender: TObject; const UserID: String; var AuthMethods: String);
var
  ChangeAuthMethodsForm: TFormChangeAuthMethods;
begin
  if AuthMethods = '' then
  begin
    ChangeAuthMethodsForm := TFormChangeAuthMethods.Create(nil);
    try
      ChangeAuthMethodsForm.eUserId.Text := UserID;
      ChangeAuthMethodsForm.eAuthMethods.Text := AuthMethods;

      if ChangeAuthMethodsForm.ShowModal = mrOk then
        AuthMethods := ChangeAuthMethodsForm.eAuthMethods.Text;
    finally
      FreeAndNil(ChangeAuthMethodsForm);
    end;
  end;
end;

procedure TFormAuthenticator.DoAuthAttemptStart(Sender: TObject;
  const UserID: String; const AuthMethod: String; const RemainingAuthMethods: String);
var
  DCSettingsForm: TFormDCSettings;
begin
  if AuthMethod = 'dcauth' then
  begin
    DCSettingsForm := TFormDCSettings.Create(nil);
    try
      DCSettingsForm.eUserId.Text := UserID;
      DCSettingsForm.eKeyId.Text := FAuthenticator.ExternalCrypto.KeyID;
      DCSettingsForm.eKeySecret.Text := FAuthenticator.ExternalCrypto.KeySecret;
      DCSettingsForm.eData.Text := FAuthenticator.ExternalCrypto.Data;

      if DCSettingsForm.ShowModal = mrOk then
      begin
        FAuthenticator.ExternalCrypto.KeyID := DCSettingsForm.eKeyId.Text;
        FAuthenticator.ExternalCrypto.KeySecret := DCSettingsForm.eKeySecret.Text;
        FAuthenticator.ExternalCrypto.Data := DCSettingsForm.eData.Text;
      end;
    finally
      FreeAndNil(DCSettingsForm);
    end;
  end;
end;

procedure TFormAuthenticator.DoCustomAuthStart(Sender: TObject; const UserID: String;
  const AuthMethod: String; var AuthMethodPars: String; var AuthMethodData: String);
var
  CustomAuthStartForm: TFormCustomAuthStart;
begin
  CustomAuthStartForm := TFormCustomAuthStart.Create(nil);
  try
    CustomAuthStartForm.eUserId.Text := UserID;
    CustomAuthStartForm.eAuthMethod.Text := AuthMethod;

    if CustomAuthStartForm.ShowModal = mrOk then
      AuthMethodData := CustomAuthStartForm.eAuthMethodData.Text;
  finally
      FreeAndNil(CustomAuthStartForm);
  end;
end;

procedure TFormAuthenticator.DoAuthVerify(Sender: TObject; const UserID: String;
  const AuthMethod: String; const AuthToken: String; const AuthMethodData: String; var Valid: Boolean);
var
  AuthVerifyForm: TFormAuthVerify;
begin
  AuthVerifyForm := TFormAuthVerify.Create(nil);
  try
    AuthVerifyForm.eUserId.Text := UserID;
    AuthVerifyForm.eAuthMethod.Text := AuthMethod;
    AuthVerifyForm.eAuthMethodData.Text := AuthMethodData;
    AuthVerifyForm.eAuthToken.Text := AuthToken;

    if AuthVerifyForm.ShowModal = mrOk then
      Valid := true
    else
      Valid := false;
  finally
      FreeAndNil(AuthVerifyForm);
  end;
end;

procedure TFormAuthenticator.DoAuthAttemptResult(Sender: TObject; const UserID: String;
  const AuthMethod: String; var AuthRes: Integer; var RemainingAuthMethods: String);
var
  ChangeAuthResultForm: TFormChangeAuthResult;
begin
  if FChangeAuthMethodsResult then
  begin
    ChangeAuthResultForm := TFormChangeAuthResult.Create(nil);
    try
      ChangeAuthResultForm.eUserId.Text := UserID;
      ChangeAuthResultForm.eAuthMethod.Text := AuthMethod;
      ChangeAuthResultForm.eAuthMethods.Text := RemainingAuthMethods;

      case AuthRes of
        1: // Succeeded
          ChangeAuthResultForm.rbSucceeded.Checked := true;
        2: // Failed
          ChangeAuthResultForm.rbFailed.Checked := true;
        else
          ChangeAuthResultForm.rbFurtherAuthNeeded.Checked := true;
      end;

      if ChangeAuthResultForm.ShowModal = mrOk then
      begin
        RemainingAuthMethods := ChangeAuthResultForm.eAuthMethods.Text;

        if ChangeAuthResultForm.rbSucceeded.Checked then
          AuthRes := 1
        else
        if ChangeAuthResultForm.rbFailed.Checked then
          AuthRes := 2
        else
          AuthRes := 0;
      end;
    finally
      FreeAndNil(ChangeAuthResultForm);
    end;
  end;
end;

end.











