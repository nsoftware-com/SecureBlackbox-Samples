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
unit kmipserverf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ImgList, ComCtrls, ExtCtrls, ToolWin,
  SBxTypes, SBxCore, SBxCertificateManager, SBxKMIPServer, SBxUserManager;

type
  TFormKmipserver = class(TForm)
    ToolBar: TToolBar;
    btnStart: TToolButton;
    btnStop: TToolButton;
    btnDelim1: TToolButton;
    btnDelim2: TToolButton;
    pClient: TPanel;
    ImageListLog: TImageList;
    ImageListClients: TImageList;
    ImageListToolbar: TImageList;
    lvLog: TListView;
    btSettings: TToolButton;
    Panel1: TPanel;
    Label10: TLabel;
    lvRequests: TListView;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btSettingsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Server: TsbxKMIPServer;

    procedure Server_Request(Sender: TObject; ConnectionId: Int64;
      const Operation: String; const Username: String; var Reject: Boolean);
    procedure Server_AuthAttempt(Sender: TObject; ConnectionId: Int64;
      const Username: String; const Password: String; var Allow: Boolean);
    procedure Server_Error(Sender: TObject; ErrorCode: integer; const Description: string);

    procedure Log(const AMessage: String; Error: boolean);
  public
    { Public declarations }
  end;

var
  FormKmipserver: TFormKmipserver;

implementation

{$R *.dfm}

uses settingsf;

procedure TFormKmipserver.btnStartClick(Sender: TObject);
begin
  Server.Start;
end;

procedure TFormKmipserver.btnStopClick(Sender: TObject);
begin
  Server.Stop;

  lvRequests.Clear;
end;

procedure TFormKmipserver.btSettingsClick(Sender: TObject);
var
  i: integer;
  Settings: TFormsettings;
  CertificateManager: TsbxCertificateManager;
begin
  Settings := TFormsettings.Create(nil);
  try
    Settings.UpdateSettings(Server);

    if Settings.ShowModal = mrOk then
    begin
      Server.StorageFileName := Settings.edStorageFile.Text;

      case Settings.rgEncoderType.ItemIndex of
        1: Server.EncoderType := etXML;

        2: Server.EncoderType := etJSON;

        else Server.EncoderType := etTTLV;
      end;

      Server.Port := StrToIntDef(Settings.edListenPort.Text, 5696);

      if Settings.cbUseSSL.Checked then
        Server.Config('SSLMode=true')
      else
        Server.Config('SSLMode=false');

      if Settings.cbUseCompression.Checked then
        Server.Config('UseCompression=true')
      else
        Server.Config('UseCompression=false');

      if Settings.cbUseChunking.Checked then
        Server.Config('UseChunkedTransfer=true')
      else
        Server.Config('UseChunkedTransfer=false');

      if Settings.cbBasicAuth.Checked then
        Server.Config('AuthBasic=true')
      else
        Server.Config('AuthBasic=false');

      if Settings.cbDigestAuth.Checked then
        Server.Config('AuthDigest=true')
      else
        Server.Config('AuthDigest=false');

      Server.Users.Clear;
      for i := 0 to Settings.UserManager.Users.Count - 1 do
        Server.Users.Add(Settings.UserManager.Users[i]);

      if (Settings.edCertFile.Text <> '') and FileExists(Settings.edCertFile.Text) then
      begin
        CertificateManager := TsbxCertificateManager.Create(nil);
        try
          try
            CertificateManager.ImportFromFile(Settings.edCertFile.Text, Settings.edCertPassword.Text);

            Server.CACertificate := CertificateManager.Certificate;
          except
            on E: Exception do
              MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
          end;
        finally
          FreeAndNil(CertificateManager);
        end;
      end;
    end;
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TFormKmipserver.FormCreate(Sender: TObject);
var
  i: integer;
  UserManager: TsbxUserManager;
begin
  IsMultiThread := true;
  
  Server := TsbxKMipServer.Create(nil);

  Server.OnRequest := Server_Request;
  Server.OnAuthAttempt := Server_AuthAttempt;
  Server.OnError := Server_Error;

  UserManager := TsbxUserManager.Create(nil);
  try
    /// NEVER USE THIS PASSWORD IN REAL PROJECT. IT IS NOT SAFE. EVERYONE KNOWS IT !!!!!!!!!!!!!!
    UserManager.Load('Users.dat', 'Asd$%sdf####f.>');
    for i := 0 to UserManager.Users.Count - 1 do
      Server.Users.Add(UserManager.Users.Item[i]);
  finally
    FreeAndNil(UserManager);
  end;
end;

procedure TFormKmipserver.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Server);
end;

procedure TFormKmipserver.Server_Request(Sender: TObject; ConnectionId: Int64;
  const Operation: String; const Username: String; var Reject: Boolean);
var
  Item : TListItem;
begin
  Item := lvRequests.Items.Add;
  Item.Caption := Operation;
  Item.SubItems.Add(Username);

  Reject := false;
end;

procedure TFormKmipserver.Server_AuthAttempt(Sender: TObject; ConnectionId: Int64;
  const Username: String; const Password: String; var Allow: Boolean);
begin
  if Server.Users.Count = 0 then
    Allow := true;
end;

procedure TFormKmipserver.Server_Error(Sender: TObject; ErrorCode: integer; const Description: string);
var
  S: string;
begin
  S := 'Error code: ' + IntToStr(ErrorCode);
  if Length(Description) > 0 then
    S := S + '. Description: ' + Description;

  Log(S, true);
end;

procedure TFormKmipserver.Log(const AMessage: String; Error: boolean);
var
  Item : TListItem;
begin
  Item := lvLog.Items.Insert(0);
  Item.Caption := TimeToStr(Now);
  if Error then
    Item.ImageIndex := 1
  else
    Item.ImageIndex := 0;
  Item.SubItems.Add(AMessage);
end;

end.











