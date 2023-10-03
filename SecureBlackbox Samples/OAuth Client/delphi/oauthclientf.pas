(*
 * SecureBlackbox 2022 Delphi Edition - Sample Project
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
unit oauthclientf;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls;

type
  TFormOAuthClient = class(TForm)
    lblInfo: TLabel;
    grpParams: TGroupBox;
    grpGrantType: TRadioGroup;
    lblAuthUrl: TLabel;
    edtAuthUrl: TEdit;
    lblTokenUrl: TLabel;
    edtTokenUrl: TEdit;
    lblClientId: TLabel;
    edtClientId: TEdit;
    lblClientSecret: TLabel;
    edtClientSecret: TEdit;
    lblScope: TLabel;
    edtScope: TEdit;
    lblRedirectUrl: TLabel;
    edtRedirectUrl: TEdit;
    lblRefreshToken: TLabel;
    edtRefreshToken: TEdit;
    lblUsername: TLabel;
    edtUsername: TEdit;
    lblPassword: TLabel;
    edtPassword: TEdit;
    grpResults: TGroupBox;
    btnAuthorize: TButton;
    lblAccessToken: TLabel;
    edtAccessToken: TEdit;
    lblExpiration: TLabel;
    edtExpiration: TEdit;
    lblTokenType: TLabel;
    edtTokenType: TEdit;
    lblNewRefreshToken: TLabel;
    edtNewRefreshToken: TEdit;
    procedure grpGrantTypeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnAuthorizeClick(Sender: TObject);
  private
    procedure LoadSettings();
    procedure SaveSettings();
  public
    { Public declarations }
  end;

var
  FormOAuthClient: TFormOAuthClient;

implementation

{$R *.dfm}

uses
  IniFiles, sbxTypes, sbxOAuthClient;

const
  IniSection = 'OAuthClient';

function RemoveSquareBrackets(const Value: string): string;
begin
  if (Value = '') or
     (Value[1] = '[') and (Value[Length(Value)] = ']') then
    Result := ''
  else
    Result := Value;
end;

{ TFormOAuthClient }

procedure TFormOAuthClient.btnAuthorizeClick(Sender: TObject);
var
  Client: TsbxOAuthClient;
begin
  edtAccessToken.Text := '';
  edtExpiration.Text := '';
  edtTokenType.Text := '';
  if edtNewRefreshToken.Text <> '' then
  begin
    edtRefreshToken.Text := edtNewRefreshToken.Text;
    edtNewRefreshToken.Text := '';
  end;

  Client := TsbxOAuthClient.Create(nil);
  try
    Client.AuthURL := edtAuthUrl.Text;
    Client.TokenURL := edtTokenUrl.Text;
    Client.Scope := edtScope.Text;
    Client.RefreshToken := edtRefreshToken.Text;

    case grpGrantType.ItemIndex of
      0:  // authorization code
          begin
            Client.GrantType := ogtAuthorizationCode;
            Client.ClientID := edtClientId.Text;
            Client.ClientSecret := edtClientSecret.Text;
            Client.RedirectURL := edtRedirectUrl.Text;
          end;

      1:  // implicit
          begin
            Client.GrantType := ogtImplicit;
            Client.ClientID := edtClientId.Text;
            Client.ClientSecret := edtClientSecret.Text;
            Client.RedirectURL := edtRedirectUrl.Text;
          end;

      2:  // password credentials
          begin
            Client.GrantType := ogtPasswordCredentials;
            Client.Username := edtUsername.Text;
            Client.Password := edtPassword.Text;
          end;

      3:  // client credentials
          begin
            Client.GrantType := ogtClientCredentials;
            Client.ClientID := edtClientId.Text;
            Client.ClientSecret := edtClientSecret.Text;
          end;
    end;

    try
      Client.Authorize();

      edtAccessToken.Text := Client.AccessToken;
      edtExpiration.Text := Client.ExpiresAt;
      edtTokenType.Text := Client.TokenType;
      if Client.RefreshToken <> edtRefreshToken.Text then
        edtNewRefreshToken.Text := Client.RefreshToken
      else
        edtNewRefreshToken.Text := '';
    except
      on E: Exception do
        MessageBox(Self.Handle, PChar('Failed to authorize the user.'#13#10'[' + E.ClassName + '] ' + E.Message), PChar(Self.Caption), MB_ICONERROR or MB_OK);
    end;
  finally
    FreeAndNil(Client);
  end;
end;

procedure TFormOAuthClient.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings();
end;

procedure TFormOAuthClient.FormCreate(Sender: TObject);
begin
  LoadSettings();
  grpGrantTypeClick(nil);
end;

procedure TFormOAuthClient.grpGrantTypeClick(Sender: TObject);
begin
  case grpGrantType.ItemIndex of
    0:  // authorization code
        begin
          lblUsername.Enabled := False;
          edtUsername.Enabled := False;
          lblPassword.Enabled := False;
          edtPassword.Enabled := False;

          lblAuthUrl.Enabled := True;
          edtAuthUrl.Enabled := True;
          lblTokenUrl.Enabled := True;
          edtTokenUrl.Enabled := True;
          lblClientId.Enabled := True;
          edtClientId.Enabled := True;
          lblClientSecret.Enabled := True;
          edtClientSecret.Enabled := True;
          lblRedirectUrl.Enabled := True;
          edtRedirectUrl.Enabled := True;
        end;

    1:  // implicit
        begin
          lblTokenUrl.Enabled := False;
          edtTokenUrl.Enabled := False;
          lblClientSecret.Enabled := False;
          edtClientSecret.Enabled := False;
          lblUsername.Enabled := False;
          edtUsername.Enabled := False;
          lblPassword.Enabled := False;
          edtPassword.Enabled := False;

          lblAuthUrl.Enabled := True;
          edtAuthUrl.Enabled := True;
          lblClientId.Enabled := True;
          edtClientId.Enabled := True;
          lblRedirectUrl.Enabled := True;
          edtRedirectUrl.Enabled := True;
        end;

    2:  // password credentials
        begin
          lblAuthUrl.Enabled := False;
          edtAuthUrl.Enabled := False;
          lblClientId.Enabled := False;
          edtClientId.Enabled := False;
          lblClientSecret.Enabled := False;
          edtClientSecret.Enabled := False;
          lblRedirectUrl.Enabled := False;
          edtRedirectUrl.Enabled := False;

          lblTokenUrl.Enabled := True;
          edtTokenUrl.Enabled := True;
          lblUsername.Enabled := True;
          edtUsername.Enabled := True;
          lblPassword.Enabled := True;
          edtPassword.Enabled := True;
        end;

    3:  // client credentials
        begin
          lblAuthUrl.Enabled := False;
          edtAuthUrl.Enabled := False;
          lblRedirectUrl.Enabled := False;
          edtRedirectUrl.Enabled := False;
          lblUsername.Enabled := False;
          edtUsername.Enabled := False;
          lblPassword.Enabled := False;
          edtPassword.Enabled := False;

          lblTokenUrl.Enabled := True;
          edtTokenUrl.Enabled := True;
          lblClientId.Enabled := True;
          edtClientId.Enabled := True;
          lblClientSecret.Enabled := True;
          edtClientSecret.Enabled := True;
        end;
  end;
end;

procedure TFormOAuthClient.LoadSettings();
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    grpGrantType.ItemIndex := Ini.ReadInteger(IniSection, 'GrantType', 0);
    grpGrantTypeClick(nil);
    edtAuthUrl.Text := Ini.ReadString(IniSection, 'AuthUrl', edtAuthUrl.Text);
    edtTokenUrl.Text := Ini.ReadString(IniSection, 'TokenUrl', edtTokenUrl.Text);
    edtClientId.Text := Ini.ReadString(IniSection, 'ClientId', edtClientId.Text);
    edtClientSecret.Text := Ini.ReadString(IniSection, 'ClientSecret', edtClientSecret.Text);
    edtScope.Text := Ini.ReadString(IniSection, 'Scope', edtScope.Text);
    edtRedirectUrl.Text := Ini.ReadString(IniSection, 'RedirectUrl', edtRedirectUrl.Text);
    edtRefreshToken.Text := Ini.ReadString(IniSection, 'RefreshToken', edtRefreshToken.Text);
    edtUsername.Text := Ini.ReadString(IniSection, 'Username', edtUsername.Text);
    edtPassword.Text := Ini.ReadString(IniSection, 'Password', edtPassword.Text);
  finally
    FreeAndNil(Ini);
  end;
end;

procedure TFormOAuthClient.SaveSettings();
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    Ini.WriteInteger(IniSection, 'GrantType', grpGrantType.ItemIndex);
    Ini.WriteString(IniSection, 'AuthUrl', edtAuthUrl.Text);
    Ini.WriteString(IniSection, 'TokenUrl', edtTokenUrl.Text);
    Ini.WriteString(IniSection, 'ClientId', RemoveSquareBrackets(edtClientId.Text));
    Ini.WriteString(IniSection, 'ClientSecret', RemoveSquareBrackets(edtClientSecret.Text));
    Ini.WriteString(IniSection, 'Scope', edtScope.Text);
    Ini.WriteString(IniSection, 'RedirectUrl', RemoveSquareBrackets(edtRedirectUrl.Text));
    if edtNewRefreshToken.Text <> '' then
      Ini.WriteString(IniSection, 'RefreshToken', edtNewRefreshToken.Text)
    else
      Ini.WriteString(IniSection, 'RefreshToken', edtRefreshToken.Text);
    Ini.WriteString(IniSection, 'Username', edtUsername.Text);
    Ini.WriteString(IniSection, 'Password', edtPassword.Text);
  finally
    FreeAndNil(Ini);
  end;
end;

end.

