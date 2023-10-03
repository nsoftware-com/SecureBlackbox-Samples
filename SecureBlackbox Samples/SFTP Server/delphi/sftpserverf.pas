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
unit sftpserverf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ImgList, ComCtrls, ExtCtrls, ToolWin,
  SBxTypes, SBxCore, SBxSFTPServer, SBxUserManager, SBxSSHKeyManager;

type
  TFormSftpserver = class(TForm)
    ToolBar: TToolBar;
    btnStart: TToolButton;
    btnStop: TToolButton;
    btnDelim1: TToolButton;
    btnDelim2: TToolButton;
    pClient: TPanel;
    lvConnections: TListView;
    ImageListLog: TImageList;
    ImageListClients: TImageList;
    ImageListToolbar: TImageList;
    lvLog: TListView;
    btSettings: TToolButton;
    Panel1: TPanel;
    Label10: TLabel;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btSettingsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Server: TsbxSFTPServer;

    procedure Server_Accept(Sender: TObject; const RemoteAddress: string; RemotePort: Integer; var Accept: Boolean);
    procedure Server_Connect(Sender: TObject; SessionId: Int64; const RemoteAddress: string; RemotePort: Integer);
    procedure Server_AuthAttempt(Sender: TObject; ConnectionID: Int64; const Username: string; AuthType: integer; var Accept: Boolean);
    procedure Server_AuthFailed(Sender: TObject; ConnectionID: Int64; const Username: string; AuthType: integer);
    procedure Server_AuthSucceeded(Sender: TObject; ConnectionID: Int64; const Username: string; AuthType: integer);
    procedure Server_SessionEstablished(Sender: TObject; ConnectionID: Int64);
    procedure Server_SessionClosed(Sender: TObject; ConnectionID: Int64);
    procedure Server_Disconnect(Sender: TObject; ConnectionID: Int64);
    procedure Server_Error(Sender: TObject; ErrorCode: integer; const Description: string);
    procedure Log(const AMessage: String);
    procedure LoadPrivateKey;
  public
    { Public declarations }
  end;

var
  FormSftpserver: TFormSftpserver;

implementation

{$R *.dfm}

uses settingsf;

procedure TFormSftpserver.btnStartClick(Sender: TObject);
begin
  Server.Start;
  Log('Server started');
end;

procedure TFormSftpserver.btnStopClick(Sender: TObject);
begin
  Server.Stop;

  lvConnections.Clear;
  Log('Server stoped');
end;

procedure TFormSftpserver.btSettingsClick(Sender: TObject);
begin
  TFormsettings.Execute(Server);
end;

procedure TFormSftpserver.FormCreate(Sender: TObject);
var
  i: integer;
  UserManager: TsbxUserManager;
begin
  IsMultiThread := true;
  
  Server := TsbxSFTPServer.Create(nil);

  LoadPrivateKey;
  Server.BaseDir := 'D:\'; // Current dir

  Server.OnAccept := Server_Accept;
  Server.OnConnect := Server_Connect;
  Server.OnAuthAttempt := Server_AuthAttempt;
  Server.OnAuthFailed := Server_AuthFailed;
  Server.OnAuthSucceeded := Server_AuthSucceeded;
  Server.OnSessionEstablished := Server_SessionEstablished;
  Server.OnSessionClosed := Server_SessionClosed;
  Server.OnDisconnect := Server_Disconnect;
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

procedure TFormSftpserver.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Server);
end;

procedure TFormSftpserver.Server_Accept(Sender: TObject; const RemoteAddress: string; RemotePort: Integer; var Accept: Boolean);
begin
  Log('Try connection from ' + RemoteAddress + ':' + IntToStr(RemotePort));
  Accept := true;
end;

procedure TFormSftpserver.Server_Connect(Sender: TObject; SessionId: Int64; const RemoteAddress: string; RemotePort: Integer);
var
  Item: TListItem;
begin
  Item := lvConnections.Items.Add;
  Item.Caption := RemoteAddress + ':' + IntToStr(RemotePort);
  Item.SubItems.Add('anonymous');
  Item.SubItems.Add('handshaking');
  Item.SubItems.Add(DateTimeToStr(Now()));
  Item.Data := Pointer(SessionId);
  Item.ImageIndex := 0;

  Log('User from ' + RemoteAddress + ' connected');
end;

procedure TFormSftpserver.Server_AuthAttempt(Sender: TObject; ConnectionID: Int64; const Username: string; AuthType: integer; var Accept: Boolean);
begin
  Log('Auth ' + IntToStr(AuthType) + ' by User ' + Username);
  Accept := true;
end;

procedure TFormSftpserver.Server_AuthFailed(Sender: TObject; ConnectionID: Int64; const Username: string; AuthType: integer);
begin
  Log('Auth ' + IntToStr(AuthType) + ' by User ' + Username + ' failed');
end;

procedure TFormSftpserver.Server_AuthSucceeded(Sender: TObject; ConnectionID: Int64; const Username: string; AuthType: integer);
begin
  Log('Auth ' + IntToStr(AuthType) + ' by User ' + Username + ' succeeded');
end;

procedure TFormSftpserver.Server_SessionEstablished(Sender: TObject; ConnectionID: Int64);
var
  i: integer;
begin
  for i := 0 to lvConnections.Items.Count - 1 do
  begin
    if Int64(lvConnections.Items[i].Data) = ConnectionID then
    begin
      lvConnections.Items[i].SubItems[1] := 'connected';
      Break;
    end;
  end;

  Log('Session ' + IntToStr(ConnectionID) + ' established');
end;

procedure TFormSftpserver.Server_SessionClosed(Sender: TObject; ConnectionID: Int64);
var
  i: integer;
begin
  for i := 0 to lvConnections.Items.Count - 1 do
  begin
    if Int64(lvConnections.Items[i].Data) = ConnectionID then
    begin
      lvConnections.Items[i].SubItems[1] := 'disconnected';
      Break;
    end;
  end;

  Log('Session ' + IntToStr(ConnectionID) + ' closed');
end;

procedure TFormSftpserver.Server_Disconnect(Sender: TObject; ConnectionID: Int64);
var
  i: integer;
  remoteAddress: string;
begin
  for i := 0 to lvConnections.Items.Count - 1 do
  begin
    if Int64(lvConnections.Items[i].Data) = ConnectionID then
    begin
      remoteAddress := lvConnections.Items[i].SubItems[0];
      lvConnections.Items.Delete(i);
      Break;
    end;
  end;

  Log('Close connection form ' + remoteAddress);
end;

procedure TFormSftpserver.Server_Error(Sender: TObject; ErrorCode: integer; const Description: string);
var
  S: string;
begin
  S := 'Error code: ' + IntToStr(ErrorCode);
  if Length(Description) > 0 then
    S := S + '. Description: ' + Description;

  Log(S);
end;


procedure TFormSftpserver.LoadPrivateKey;
var
  KeyManager: TsbxSSHKeyManager;
  S: string;
begin
  if Server.ServerKeys.Count = 0 then
  begin
    /// NEVER USE THIS KEY IN REAL PROJECT. IT IS NOT SAFE. EVERYONE KNOWS IT !!!!!!!!!!!!!!
    S := '-----BEGIN RSA PRIVATE KEY-----'#13#10 +
      'Proc-Type: 4,ENCRYPTED'#13#10 +
      'DEK-Info: DES-EDE3-CBC,93CB1D7A8ADEBCB0'#13#10 +
      ''#13#10 +
      '+GrcTA8Ty1PznAiUAH7l20LjDM10TXZAC5IzUpdh+1YDgmLm+2OEYnBAbkdJbWVZ'#13#10 +
      'wmS8ZHAI8qaTrNtWXbAdPKA0Psll3vVoik9/7m4sCmoxPq44R1cEMnbXzd6tkbkJ'#13#10 +
      'cgBukWnR8Loz7Ynbri0edTCkpptSeRt14authVN48GTlhel7G3EDFMiYY+OITDCh'#13#10 +
      'kPSjp55y+tMBiREXlLFLz9s8+B7uI/IDiXCGoO38O3ZtBFsnFsZE5i9cdADd3WZf'#13#10 +
      'q05ylNJvCh9rBEXWpaKIKVzvasdQ9qeS1suzXssph02cQ7D+oK7AW3934jIZSbRQ'#13#10 +
      'Q1DH8ebnikAOSLnEKEik9aMdwX6sKS8ppHGpk4x83clu79biftKAWoAr5DEGquPV'#13#10 +
      'KMUZBzcA+MwRBGg/pXGQ+y53Xaw49Aax6Xnbm9vpaGwUWWlgAoOrEUJdh/gH++sy'#13#10 +
      'JlosoynUky4F3xdvT/nbS1duE39Y6/k6GB6cuQIhtguvTQpvW0pxZb9QE49ePecP'#13#10 +
      'rYg18lABw4YirEELIfSwkw7iCiQC+bzkcivQf7yA/chx6d6vYJjimSIE5D3oOhti'#13#10 +
      '+doXBdoH4uydXk7zhHZjT/6C/UYlfohvJMpOhkOH40fyckzlTLlkei1Iwqqd/BLA'#13#10 +
      'rCazQGZGBF+jP1Cls+Dug/alVSVW3mrv6DlrgYkAf4bmZh//3P3UyY3cgkbVUwWZ'#13#10 +
      'Vi0PqgOFE9VWblsoXZMwuL7ChZIhjUZ/ky7zRvN5KVMHmBQrk+LEpAp6j/6rcbTR'#13#10 +
      'wkOXdR/GDKKNN8iBgo2IF+iRUOAw1X+W4BFBlG6bKocOAzD3IGqF5A=='#13#10 +
      '-----END RSA PRIVATE KEY-----'#13#10;

    try
      KeyManager := TsbxSSHKeyManager.Create(nil);
      try
        KeyManager.ImportBytes(TEncoding.UTF8.GetBytes(S), 'password');

        Server.ServerKeys.Add(KeyManager.Key);
      finally
        FreeAndNil(KeyManager);
      end;
    except
      on E: Exception do
        MessageDlg(E.Message, mtError, [mbOk], 0);
    end;
  end;
end;

procedure TFormSftpserver.Log(const AMessage: String);
var
  Item : TListItem;
begin
  Item := lvLog.Items.Insert(0);
  Item.Caption := TimeToStr(Now);
  Item.ImageIndex := 0;
  Item.SubItems.Add(AMessage);
end;

end.







