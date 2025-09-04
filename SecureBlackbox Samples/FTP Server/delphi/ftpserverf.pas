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
unit ftpserverf;

interface

uses
  Windows, Messages, SysUtils, StdCtrls, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ToolWin, ComCtrls, ExtCtrls, ScktComp, ImgList, SyncObjs,
  SBxTypes, SBxCore, SBxCertificateManager, SBxFTPServer, SBxUserManager;

const
  UM_ADDITEM = WM_APP + 1;
  UM_UPDATEITEM = WM_APP + 2;
  UM_REMOVEITEM = WM_APP + 3;
  UM_LOG = WM_APP + 4;

type
  TUMListItem = record
    ConnectionID: Int64;
    RemoteAddress: string;
    UserName: string;
    CurrentDir: string;
    Operation: string;
    Progress: string;
    LogMessage: string;
  end;

  PUMListItem = ^TUMListItem;

  TFormFtpserver = class(TForm)
    ToolBar: TToolBar;
    btnStart: TToolButton;
    btnStop: TToolButton;
    btnDelim1: TToolButton;
    btnAuthSettings: TToolButton;
    pLog: TPanel;
    Splitter: TSplitter;
    pClient: TPanel;
    lvLog: TListView;
    lvConnections: TListView;
    btnServerSettings: TToolButton;
    StatusBar: TStatusBar;
    ImageListLog: TImageList;
    ImageListClients: TImageList;
    ImageListToolbar: TImageList;
    btnDelim2: TToolButton;
    btnKillConn: TToolButton;
    Panel1: TPanel;
    Label10: TLabel;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnAuthSettingsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnServerSettingsClick(Sender: TObject);
    procedure mnuStartClick(Sender: TObject);
    procedure mnuStopClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure btnKillConnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    Server: TsbxFTPServer;

    FFinalizing : boolean;

    procedure Server_Error(Sender: TObject; ConnectionID: Int64; ErrorCode: Integer;
      Fatal: Boolean; Remote: Boolean; const Description: String);
    procedure Server_Accept(Sender: TObject; const RemoteAddress: string;
      RemotePort: integer; var Accept: Boolean);
    procedure Server_Connect(Sender: TObject; ConnectionID: Int64; const RemoteAddress: string; RemotePort: integer);
    procedure Server_AuthAttempt(Sender: TObject; ConnectionID: Int64; const Username, Password: string;
      var Allow: Boolean; var BasePath: String);
    procedure Server_CommandReceived(Sender: TObject; ConnectionID: Int64; const Command, Parameters: string; var Ignore: Boolean);
    procedure Server_CommandProcessed(Sender: TObject; ConnectionID: Int64; const CurrentDirectory: string;
      const Command: string; ReplyCode: integer);
    procedure Server_BeforeSendReply(Sender: TObject; ConnectionID: Int64; var Code: Integer;
      var Reply: String; const Command: string);
    procedure Server_Disconnect(Sender: TObject; ConnectionID: Int64);

    procedure LogMes(AMessage: string);
    procedure Log(var Msg: TMessage); message UM_LOG;
    procedure AddClientToList(var Msg: TMessage); message UM_ADDITEM;
    procedure UpdateClientInList(var Msg: TMessage); message UM_UPDATEITEM;
    procedure RemoveClientFromList(var Msg: TMessage); message UM_REMOVEITEM;
  public
    procedure Initialize;
    procedure Finalize;
    procedure Start;
    procedure Stop;
    procedure ExitApp;
    procedure AuthSettings;
    procedure ServerSettings;
  end;

  TFTPSServerDemoSettings = class
  private
    FCertificateFile: string;
    FCertificatePassword : string;
    FImplicitSSL : boolean;
    FRequireTLS : boolean;
    FRequireTLSForData : boolean;
    FSettingsFound : boolean;
    FAllowAnonymous : boolean;
    FListeningPort : integer;
    FPassiveModeHost : string;
    FPortRangeFrom : integer;
    FPortRangeTo : integer;

    procedure LoadDefaults;
    function GetSettingsFilename : string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;

    property CertificateFile : string read FCertificateFile write FCertificateFile;
    property CertificatePassword : string read FCertificatePassword write FCertificatePassword;
    property ImplicitSSL : boolean read FImplicitSSL write FImplicitSSL;
    property RequireTLS : boolean read FRequireTLS write FRequireTLS;
    property RequireTLSForData : boolean read FRequireTLSForData write FRequireTLSForData;
    property AllowAnonymous : boolean read FAllowAnonymous write FAllowAnonymous;
    property ListeningPort : integer read FListeningPort write FListeningPort;
    property PassiveModeHost : string read FPassiveModeHost write FPassiveModeHost;
    property PortRangeFrom : integer read FPortRangeFrom write FPortRangeFrom;
    property PortRangeTo : integer read FPortRangeTo write FPortRangeTo;

    property SettingsFound: Boolean read FSettingsFound;
  end;

var
  FormFtpserver: TFormFtpserver;
  Settings: TFTPSServerDemoSettings;

implementation

uses
  ConnPropsF,
  AuthSettingsF,
  ServerSettingsF;

{$R *.DFM}

procedure TFormFtpserver.Initialize;
var
  i: integer;
  UserManager: TsbxUserManager;
begin
  Settings := TFTPSServerDemoSettings.Create;
  Settings.Load;

  StatusBar.Panels[0].Text := 'Inactive';
  FFinalizing := false;

  Server := TsbxFTPServer.Create(nil);
  Server.OnError := Server_Error;
  Server.OnAccept := Server_Accept;
  Server.OnConnect := Server_Connect;
  Server.OnAuthAttempt := Server_AuthAttempt;
  Server.OnCommandReceived := Server_CommandReceived;
  Server.OnCommandProcessed := Server_CommandProcessed;
  Server.OnBeforeSendReply := Server_BeforeSendReply;
  Server.OnDisconnect := Server_Disconnect;

  UserManager := TsbxUserManager.Create(nil);
  try
    /// NEVER USE THIS PASSWORD IN REAL PROJECT. IT IS NOT SAFE. EVERYONE KNOWS IT !!!!!!!!!!!!!!
    UserManager.ImportFromFile('Users.dat', '@WE$rt65', true);
    for i := 0 to UserManager.Users.Count - 1 do
      Server.Users.Add(UserManager.Users.Item[i]);
  finally
    FreeAndNil(UserManager);
  end;
end;

procedure TFormFtpserver.Finalize;
var
  I : integer;
begin
  FFinalizing := true;
  if Server.Active then
    Stop;

  FreeAndNil(Server);

  FreeAndNil(Settings);
end;

procedure TFormFtpserver.Server_Error(Sender: TObject; ConnectionID: Int64; ErrorCode: Integer;
  Fatal: Boolean; Remote: Boolean; const Description: String);
var
  Item: PUMListItem;
begin
  if FFinalizing then
    exit;

  New(Item);
  Item^.LogMessage := 'Error: ' + Description;

  PostMessage(Self.Handle, UM_LOG, 0, Integer(Item));
end;

procedure TFormFtpserver.Server_Accept(Sender: TObject; const RemoteAddress: string;
  RemotePort: integer; var Accept: Boolean);
begin
  Accept := true;
end;

procedure TFormFtpserver.Server_Connect(Sender: TObject; ConnectionID: Int64;
  const RemoteAddress: string; RemotePort: integer);
var
  Item: PUMListItem;
begin
  if FFinalizing then
    exit;

  New(Item);
  Item^.ConnectionID := ConnectionID;
  Item^.RemoteAddress := RemoteAddress + ':' + IntToStr(RemotePort);
  PostMessage(Self.Handle, UM_ADDITEM, 0, Integer(Item));

  New(Item);
  Item^.LogMessage := 'Client connected from ' + RemoteAddress + ':' + IntToStr(RemotePort);
  PostMessage(Self.Handle, UM_LOG, 0, Integer(Item));
end;

procedure TFormFtpserver.Server_AuthAttempt(Sender: TObject; ConnectionID: Int64;
  const Username, Password: string; var Allow: Boolean; var BasePath: String);
var
  Item: PUMListItem;
begin
  if FFinalizing then
    exit;

  if CompareStr(LowerCase(Username), 'anonymous') = 0 then
  begin
    Allow := Settings.AllowAnonymous;
    if Allow then
    begin
      New(Item);
      Item^.LogMessage := 'Access granted to anonymous user (' + Password + ')';
      PostMessage(Self.Handle, UM_LOG, 0, Integer(Item));
    end
    else
    begin
      New(Item);
      Item^.LogMessage := 'Access denied for anonymous user (' + Password + ')';
      PostMessage(Self.Handle, UM_LOG, 0, Integer(Item));
    end;
  end
  else
  begin
    if Allow then
    begin
      New(Item);
      Item^.LogMessage := 'Access granted to user ' + Username;
      PostMessage(Self.Handle, UM_LOG, 0, Integer(Item));
    end
    else
    begin
      New(Item);
      Item^.LogMessage := 'Access denied for user ' + Username;
      PostMessage(Self.Handle, UM_LOG, 0, Integer(Item));
    end;
  end;

  New(Item);
  Item^.ConnectionID := ConnectionID;
  Item^.UserName := Username;
  Item^.CurrentDir := '-';
  Item^.Operation := '-';
  Item^.Progress := '-';
  PostMessage(Self.Handle, UM_UPDATEITEM, 0, Integer(Item));
end;

procedure TFormFtpserver.Server_CommandReceived(Sender: TObject; ConnectionID: Int64;
  const Command, Parameters: string; var Ignore: Boolean);
var
  Item: PUMListItem;
begin
  if FFinalizing then
    exit;

  New(Item);
  Item^.ConnectionID := ConnectionID;
  Item^.UserName := '-';
  Item^.CurrentDir := '-';
  Item^.Operation := Command + ' ' + Parameters;
  Item^.Progress := '-';
  PostMessage(Self.Handle, UM_UPDATEITEM, 0, Integer(Item));

  Ignore := false;
end;

procedure TFormFtpserver.Server_CommandProcessed(Sender: TObject; ConnectionID: Int64;
  const CurrentDirectory: string; const Command: string; ReplyCode: integer);
var
  Item: PUMListItem;
begin
  if FFinalizing then
    exit;

  New(Item);
  Item^.ConnectionID := ConnectionID;
  Item^.UserName := '-';
  Item^.CurrentDir := CurrentDirectory;
  Item^.Operation := Command;
  Item^.Progress := 'Finished : ' + IntToStr(ReplyCode);
  PostMessage(Self.Handle, UM_UPDATEITEM, 0, Integer(Item));
end;

procedure TFormFtpserver.Server_BeforeSendReply(Sender: TObject; ConnectionID: Int64;
  var Code: Integer; var Reply: String; const Command: string);
begin
  //
end;

procedure TFormFtpserver.Server_Disconnect(Sender: TObject; ConnectionID: Int64);
var
  RemoteAddress: string;
  Item: PUMListItem;
begin
  if FFinalizing then
    exit;

  New(Item);
  Item^.ConnectionID := ConnectionID;
  PostMessage(Self.Handle, UM_REMOVEITEM, 0, Integer(Item));

  New(Item);
  Item^.LogMessage := 'Client from ' + RemoteAddress + ' disconnected.';
  PostMessage(Self.Handle, UM_LOG, 0, Integer(Item));
end;

procedure TFormFtpserver.Start;
var
  Opts: integer;
  CertificateManager: TsbxCertificateManager;
begin
  if not Server.Active then
  begin
    FormConnprops.cbImplicitSSL.Checked := Settings.ImplicitSSL;
    FormConnprops.editPort.Text := IntToStr(Settings.ListeningPort);

    if FormConnprops.ShowModal = mrOk then
    begin
      Settings.ImplicitSSL := FormConnprops.cbImplicitSSL.Checked;
      Server.ImplicitSSL := Settings.ImplicitSSL;
      Server.AllowAnonymous := Settings.AllowAnonymous;
      Settings.ListeningPort := StrToIntDef(FormConnprops.editPort.Text, 21);
      Server.Port := Settings.ListeningPort;
      Server.PassiveModeHost := Settings.PassiveModeHost;
      Server.DataPortRangeFrom := Settings.PortRangeFrom;
      Server.DataPortRangeTo := Settings.PortRangeTo;
      Server.ImplicitSSL := FormConnprops.cbImplicitSSL.Checked;

      Server.TLSServerChain.Clear();
      if Length(Settings.CertificateFile) > 0 then
      begin
        CertificateManager := TsbxCertificateManager.Create(nil);
        try
          try
            CertificateManager.ImportFromFile(Settings.CertificateFile, Settings.CertificatePassword);

            Server.TLSServerChain.Add(CertificateManager.Certificate);
          except
            MessageDlg('Cannot load FTP server certificate!', mtError, [mbOk], 0);
          end;
        finally
          FreeAndNil(CertificateManager);
        end;
      end;

      Opts := 1{cfsoPlainLogin} or 2{cfsoEncryption} or 4{cfsoAuth} or 8{cfsoClearControlChannel} or
        16{cfsoClearDataChannel} or 32{cfsoEncryptedDataChannel};
      if Settings.RequireTLS then
      begin
        Opts := Opts and not 1{cfsoPlainLogin};
        Opts := Opts and not 8{cfsoClearControlChannel};
      end;
      if Settings.RequireTLSForData then
        Opts := Opts and not 16{cfsoClearDataChannel};
      Server.Config('SecurityOptions=' + IntToStr(Opts));

      Server.Start;
      LogMes('Server started');
    end;
  end
  else
    MessageDlg('Server is already running', mtWarning, [mbOk], 0);
end;

procedure TFormFtpserver.Stop;
var
  I: Integer;
begin
  if Server.Active then
  begin
    FFinalizing := True;
    try
      Server.Stop;
    finally
      FFinalizing := False;
    end;

    lvConnections.Clear;
    StatusBar.Panels[0].Text := 'Inactive';
    LogMes('Server stopped');
  end
  else
    MessageDlg('Server is already halted', mtWarning, [mbOk], 0);
end;

procedure TFormFtpserver.ExitApp;
begin
  Close;
end;

procedure TFormFtpserver.AuthSettings;
var
  i: integer;
  UserManager: TsbxUserManager;
begin
  FormAuthsettings.Initialize(Server);
  FormAuthsettings.ShowModal;

  UserManager := TsbxUserManager.Create(nil);
  try
    for i := 0 to Server.Users.Count - 1 do
      UserManager.Users.Add(Server.Users.Item[i]);
    /// NEVER USE THIS PASSWORD IN REAL PROJECT. IT IS NOT SAFE. EVERYONE KNOWS IT !!!!!!!!!!!!!!
    UserManager.ExportToFile('Users.dat', '@WE$rt65');
  finally
    FreeAndNil(UserManager);
  end;
end;

procedure TFormFtpserver.ServerSettings;
begin
  FormServersettings.ShowModal;
end;

procedure TFormFtpserver.btnStartClick(Sender: TObject);
begin
  Start;
end;

procedure TFormFtpserver.btnStopClick(Sender: TObject);
begin
  Stop;
end;

procedure TFormFtpserver.btnAuthSettingsClick(Sender: TObject);
begin
  AuthSettings;
end;

procedure TFormFtpserver.btnServerSettingsClick(Sender: TObject);
begin
  ServerSettings;
end;

procedure TFormFtpserver.FormCreate(Sender: TObject);
begin
  IsMultiThread := true;

  Initialize;
end;

procedure TFormFtpserver.FormDestroy(Sender: TObject);
begin
  Finalize;
end;

procedure TFormFtpserver.AddClientToList(var Msg: TMessage);
var
  Item: TListItem;
begin
  Item := lvConnections.Items.Add;
  Item.Caption := PUMListItem(Msg.LParam)^.RemoteAddress;
  Item.SubItems.Add('');
  Item.SubItems.Add('');
  Item.SubItems.Add('');
  Item.SubItems.Add('');
  Item.Data := Pointer(PUMListItem(Msg.LParam)^.ConnectionID);

  Dispose(PUMListItem(Msg.LParam));
end;

procedure TFormFtpserver.UpdateClientInList(var Msg: TMessage);
var
  i: integer;
  Item: TListItem;
begin
  for i := 0 to lvConnections.Items.Count - 1 do
  begin
    Item := lvConnections.Items.Item[i];

    if Int64(Item.Data) = PUMListItem(Msg.LParam)^.ConnectionID then
    begin
      if PUMListItem(Msg.LParam)^.UserName <> '-' then
        Item.SubItems[0] := PUMListItem(Msg.LParam)^.UserName;
      if PUMListItem(Msg.LParam)^.CurrentDir <> '-' then
        Item.SubItems[1] := PUMListItem(Msg.LParam)^.CurrentDir;
      if PUMListItem(Msg.LParam)^.Operation <> '-' then
        Item.SubItems[2] := PUMListItem(Msg.LParam)^.Operation;
      if PUMListItem(Msg.LParam)^.Progress <> '-' then
        Item.SubItems[3] := PUMListItem(Msg.LParam)^.Progress;
    end;
  end;

  Dispose(PUMListItem(Msg.LParam));
end;

procedure TFormFtpserver.RemoveClientFromList(var Msg: TMessage);
var
  i: integer;
begin
  for i := 0 to lvConnections.Items.Count - 1 do
    if Int64(lvConnections.Items.Item[i].Data) = PUMListItem(Msg.LParam)^.ConnectionID then
    begin
      lvConnections.Items.Delete(i);
      break;
    end;

  Dispose(PUMListItem(Msg.LParam));
end;

procedure TFormFtpserver.mnuStartClick(Sender: TObject);
begin
  Start;
end;

procedure TFormFtpserver.mnuStopClick(Sender: TObject);
begin
  Stop;
end;

procedure TFormFtpserver.mnuExitClick(Sender: TObject);
begin
  ExitApp;
end;

procedure TFormFtpserver.btnKillConnClick(Sender: TObject);
begin
  if (lvConnections.Selected <> nil) and (lvConnections.Selected.Data <> nil) then
  begin
    ; // !!
  end;
end;

procedure TFormFtpserver.FormActivate(Sender: TObject);
begin
  if not Settings.SettingsFound then
  begin
    MessageDlg('Welcome to the FTPS Server Demo Application!'#13#10 +
      'Thank you for your interest in our products.'#13#10#13#10 +
      'First of all, you need to create at least one user account.'#13#10 +
      'After clicking the OK button you will be redirected to the'#13#10 +
      'User settings window.',
      mtInformation,
      [mbOk],
      0);
    FormAuthsettings.ShowModal;
  end;
end;

procedure TFormFtpserver.LogMes(AMessage: string);
var
  Item : TListItem;
begin
  Item := lvLog.Items.Insert(0);
  Item.Caption := TimeToStr(Now);
  Item.ImageIndex := 0;
  Item.SubItems.Add(AMessage);
end;

procedure TFormFtpserver.Log(var Msg: TMessage);
begin
  LogMes(PUMListItem(Msg.LParam)^.LogMessage);

  Dispose(PUMListItem(Msg.LParam));
end;

constructor TFTPSServerDemoSettings.Create;
begin
  inherited;

  FCertificateFile := '';
  FCertificatePassword := '';
  FRequireTLS := false;
  FRequireTLSForData := false;
  FImplicitSSL := false;
  FAllowAnonymous := true;
  FPassiveModeHost := '';
  FPortRangeFrom := 0;
  FPortRangeTo := 0;
end;

destructor TFTPSServerDemoSettings.Destroy;
begin
  inherited;
end;

procedure TFTPSServerDemoSettings.LoadDefaults;
begin
  FListeningPort := 21;
  FCertificateFile := '';
  FCertificatePassword := '';
  FImplicitSSL := false;
  FRequireTLS := false;
  FRequireTLSForData := false;
  FAllowAnonymous := true;
  FPassiveModeHost := '';
  FPortRangeFrom := 0;
  FPortRangeTo := 0;
  Save;
end;

function GetEnvironmentVariable(const Name: AnsiString): AnsiString;
var
  Needed: integer;
  TmpRes : string;
begin
  Needed := Windows.GetEnvironmentVariable(PChar(string(Name)), nil, 0);
  if Needed > 0 then
  begin
    SetLength(TmpRes, Needed);
    Needed := Windows.GetEnvironmentVariable(PChar(string(Name)), @TmpRes[1], Needed);
    SetLength(TmpRes, Needed);
  end
  else
    SetLength(TmpRes, 0);
  Result := AnsiString(TmpRes);
end;

function TFTPSServerDemoSettings.GetSettingsFilename: string;
var
  AppData: AnsiString;
  DirName: AnsiString;
const
  SBB_DIR_NAME = 'SecureBlackbox';
  SBB_PARAMS_FILE_NAME = 'FTPSSrvParams';
begin
  AppData := GetEnvironmentVariable('APPDATA');
  if Length(AppData) = 0 then
    AppData := GetEnvironmentVariable('USERPROFILE');
  if Length(AppData) <> 0 then
    DirName := AppData + '\' + SBB_DIR_NAME
  else
    DirName := SBB_DIR_NAME;
  if not DirectoryExists(string(DirName)) then
    if not CreateDir(string(DirName)) then
    begin
      Result := '';
      Exit;
    end;
  Result := DirName + '\' + SBB_PARAMS_FILE_NAME;
end;

procedure TFTPSServerDemoSettings.Load;
  procedure GetParamAndValue(const Line : AnsiString; var Param, Value : AnsiString);
  var
    i : integer;
  begin
    i := Pos('=', Line);
    if i > 0 then
    begin
      Param := LowerCase(Trim(Copy(Line, 1, i - 1)));
      Value := Trim(Copy(Line, i + 1, Length(Line) - i));
    end
    else
    begin
      Param := '';
      Value := '';
    end;
  end;

  function BoolForYesNo(const YesNo : AnsiString) : boolean;
  begin
    Result := (LowerCase(YesNo) = 'yes') or (LowerCase(YesNo) = 'true');
  end;
var
  SettingsFile, SettingsStr, CfgLine, Param, Value: AnsiString;
  F: TFileStream;
  i, User : integer;
  InSettings : boolean;
  Lines : TStringList;
begin
  {
    Configuration file contains a sequence of:

    [Configuration]
    ImplicitSSL=yes
    RequireTLS=yes
    RequireTLSForData=yes
    ListenPort=22
    CertificateFile=C:\Temp\cert.pfx
    CertificatePassword=password

    [User_Anonymous]
    HomeDirectory=C:\Users\Temp
    SpeedLimit=10000

    [User_Test]
    PasswordSalt=ab0b1234
    PasswordHash=ababaabababa123412424124
    HomeDirectory=C:\Users\test
    SpeedLimit=10000

    [User_Test2]
    PasswordSalt=ab0b1234
    PasswordHash=ababaabababa123412424124
    HomeDirectory=C:\Users\test2
    SpeedLimit=20000
  }
  FSettingsFound := false;
  SettingsFile := GetSettingsFilename;
  if Length(SettingsFile) > 0 then
  begin
    if FileExists(SettingsFile) then
    begin
      try
        F := TFileStream.Create(SettingsFile, fmOpenRead);
        try
          SetLength(SettingsStr, F.Size);
          F.Read(SettingsStr[1], Length(SettingsStr));
        finally
          F.Free;
        end;

        //!! process SettingsStr here
        Lines := TStringList.Create;
        Lines.SetText({$ifdef UNICODE}PWideChar(string(SettingsStr)){$else}PChar(SettingsStr){$endif});

        i := 0;
        InSettings := false;
        User := -1;

        while (i < Lines.Count) do
        begin
          CfgLine := Trim(Lines[i]);
          if LowerCase(CfgLine) = '[configuration]' then
            InSettings := true
          else if InSettings then
          begin
            GetParamAndValue(CfgLine, Param, Value);
            if Param = 'allowanonymous' then
              AllowAnonymous := BoolForYesNo(Value)
            else if Param = 'implicitssl' then
              ImplicitSSL := BoolForYesNo(Value)
            else if Param = 'requiretls' then
              RequireTLS := BoolForYesNo(Value)
            else if Param = 'requiretlsfordata' then
              RequireTLSForData := BoolForYesNo(Value)
            else if Param = 'listeningport' then
              ListeningPort := StrToIntDef(Value, 21)
            else if Param = 'certificatefile' then
              CertificateFile := Value
            else if Param = 'certificatepassword' then
              CertificatePassword := Value
            else if Param = 'passivemodehost' then
              PassiveModeHost := Value
            else if Param = 'portrangefrom' then
              PortRangeFrom := StrToIntDef(Value, 0)
            else if Param = 'portrangeto' then
              PortRangeTo := StrToIntDef(Value, 0)
          end;

          Inc(i);
        end;

        Lines.Free;

        FSettingsFound := true;
      except
        ;
      end;
    end;
  end;
  if not FSettingsFound then
    LoadDefaults;
end;

procedure TFTPSServerDemoSettings.Save;
  function YesNoForBool(Param : boolean) : string;
  begin
    if Param then
      Result := 'yes'
    else
      Result := 'no';
  end;

var
  St, SettingsFile : AnsiString;
  i : integer;
  F : TFileStream;
begin
  St := '[Configuration]'#13#10;
  St := St + 'AllowAnonymous=' + YesNoForBool(AllowAnonymous) + #13#10;
  St := St + 'ImplicitSSL=' + YesNoForBool(ImplicitSSL) + #13#10;
  St := St + 'RequireTLS=' + YesNoForBool(RequireTLS) + #13#10;
  St := St + 'RequireTLSForData=' + YesNoForBool(RequireTLSForData) + #13#10;
  St := St + 'ListeningPort=' + IntToStr(ListeningPort) + #13#10;
  St := St + 'PassiveModeHost=' + PassiveModeHost + #13#10;
  St := St + 'CertificateFile=' + CertificateFile + #13#10;
  St := St + 'CertificatePassword=' + CertificatePassword + #13#10;
  St := St + 'PortRangeFrom=' + IntToStr(PortRangeFrom) + #13#10;
  St := St + 'PortRangeTo=' + IntToStr(PortRangeTo) + #13#10;
  St := St + #13#10;

  SettingsFile := GetSettingsFilename;
  if Length(SettingsFile) > 0 then
  begin
    F := TFileStream.Create(SettingsFile, fmCreate);
    F.Write(St[1], Length(St));
    F.Free;
  end;
end;

end.
