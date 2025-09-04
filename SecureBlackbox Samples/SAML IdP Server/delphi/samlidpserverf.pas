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
unit samlidpserverf;

interface

uses
  SysUtils, Variants, Classes, Graphics, Windows, Messages,
  Controls, Forms, Dialogs, StdCtrls, Buttons, ComCtrls,
  ExtCtrls, sbxConstants, sbxTypes,
  sbxCore, sbxSAMLIdPServer, sbxCertificateManager, sbxUserManager;

const
  WM_LOG = WM_USER + 1;

type
  TFormSamlidpserver = class(TForm)
    Label16: TLabel;
    mmLog: TMemo;
    bbStart: TButton;
    bbStop: TButton;
    OpenDlg: TOpenDialog;
    SaveDlg: TSaveDialog;
    pcSteps: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    lvSessions: TListView;
    Panel1: TPanel;
    bbAddUser: TButton;
    bbRemoveUser: TButton;
    lvUsers: TListView;
    sbChooseSPMetadata: TSpeedButton;
    Label13: TLabel;
    sbChooseIDPMetadata: TSpeedButton;
    Label14: TLabel;
    Label12: TLabel;
    sbAddSP: TSpeedButton;
    edSPMetadata: TEdit;
    edIDPMetadata: TEdit;
    bbExportIDPMetadata: TBitBtn;
    lbKnownSPs: TListBox;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    sbChooseSignCert: TSpeedButton;
    sbChooseEncCert: TSpeedButton;
    sbChooseServerCert: TSpeedButton;
    Label17: TLabel;
    sbChooseMetaSignCert: TSpeedButton;
    edSignCert: TEdit;
    edEncCert: TEdit;
    edServerCert: TEdit;
    cbUseTLS: TCheckBox;
    edMetaSignCert: TEdit;
    Label1: TLabel;
    Label5: TLabel;
    edURL: TEdit;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    edSSO: TEdit;
    cbSSORedirect: TCheckBox;
    cbSSOPOST: TCheckBox;
    cbPrefSSORespBinding: TComboBox;
    cbSSOArtifact: TCheckBox;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label7: TLabel;
    Label4: TLabel;
    edSLS: TEdit;
    cbSLSRedirect: TCheckBox;
    cbSLSPOST: TCheckBox;
    cbPrefSLSRespBinding: TComboBox;
    cbSLSArtifact: TCheckBox;
    edMetadataURL: TEdit;
    mmStep1: TMemo;
    bbAutoConfig: TButton;
    mmLastStep: TMemo;
    pnStep1: TPanel;
    bbGoto2: TButton;
    Label15: TLabel;
    pnStep2: TPanel;
    Label18: TLabel;
    bbGoto3: TButton;
    pnStep3: TPanel;
    Label19: TLabel;
    bbGoto4: TButton;
    Panel2: TPanel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    memoAuthForm: TMemo;
    procedure sbAddSPClick(Sender: TObject);
    procedure bbAddUserClick(Sender: TObject);
    procedure bbRemoveUserClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bbStartClick(Sender: TObject);
    procedure bbStopClick(Sender: TObject);
    procedure bbExportIDPMetadataClick(Sender: TObject);
    procedure sbChooseSPMetadataClick(Sender: TObject);
    procedure sbChooseIDPMetadataClick(Sender: TObject);
    procedure bbAutoConfigClick(Sender: TObject);
    procedure bbGoto2Click(Sender: TObject);
    procedure sbChooseSignCertClick(Sender: TObject);
    procedure sbChooseEncCertClick(Sender: TObject);
    procedure sbChooseMetaSignCertClick(Sender: TObject);
    procedure sbChooseServerCertClick(Sender: TObject);
    procedure bbGoto3Click(Sender: TObject);
    procedure bbGoto4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FServer: TsbxSAMLIdPServer;
    FUserMgr : TsbxUserManager;
    FAutoPath: string;

    procedure AdjustServer;

    procedure DoLog(const Line : string);
    procedure DoSessionCreated(Sender: TObject; ConnectionID: Int64; const SessionID: String);
    procedure DoSessionDestroyed(Sender: TObject; ConnectionID: Int64; const SessionID: String);
    procedure DoSessionEvent(Sender: TObject; ConnectionID: Int64; const SessionID: String; const EventText: String);
    procedure DoError(Sender: TObject; ConnectionID: Int64; const SessionID: string;
      ErrorCode: Integer; Fatal, Remote: boolean; const Description: String);
    procedure DoAuthnRequestReceived(Sender: TObject; ConnectionID: Int64; const SessionID: String;
      const RequestID: String; const SP: String; IsEncrypted: Boolean; IsSigned: Boolean;
      var NameIDFormat: String; var ForceAuthn: Boolean; var NonInteractive: Boolean;
      var Authenticated: Boolean; var Action: Integer);
    procedure DoUserAuthCompleted(Sender: TObject; ConnectionID: Int64; const SessionID: String;
      const SP: String; const Auth: String; const Username: String; var NameID: String;
      var NameIDFormat: String; var SessionIndex: String; var AssertionTTL: Integer);
    procedure AsyncLog(const S: string);
    procedure LogCapture(var Msg : TMessage); message WM_LOG;

    procedure HandleCertMgrPasswordNeeded(Sender: TObject; var Password: String;
      var Cancel: Boolean);
  public
    { Public declarations }
  end;

var
  FormSamlidpserver: TFormSamlidpserver;

implementation

uses
  SHLObj, credentialsf;

{$R *.dfm}

function BrowseForFolder(var Foldr: string; Title: string): Boolean;
var
  BrowseInfo: TBrowseInfo;
  ItemIDList: PItemIDList;
  DisplayName: array[0..260] of Char;
begin
  Result := False;
  FillChar(BrowseInfo, SizeOf(BrowseInfo), #0);
  with BrowseInfo do
  begin
    hwndOwner := Application.Handle;
    pszDisplayName := @DisplayName[0];
    lpszTitle := PChar(Title);
    ulFlags := BIF_RETURNONLYFSDIRS;
  end;
  ItemIDList := SHBrowseForFolder(BrowseInfo);
  if Assigned(ItemIDList) then
    if SHGetPathFromIDList(ItemIDList, DisplayName) then
    begin
      Foldr := DisplayName;
      Result := True;
    end;

  if Result and (Foldr[Length(Foldr)] <> '\') then
    Foldr := Foldr + '\';
end;

function ExtractListenPort(const URL: string): integer;
begin
  if Pos('http://', URL) = 1 then
    Result := 80
  else if Pos('https://', URL) = 1 then
    Result := 443
  else
    raise Exception.Create('Unsupported base URL');
end;

function ExtractPort(const URL: string): integer;
var
  Idx: integer;
begin
  Idx := LastDelimiter(':', URL);
  Result := StrToIntDef(URL.Substring(Idx), 80);
end;

procedure TFormSamlidpserver.DoLog(const Line : string);
begin
  mmLog.Lines.Add(Line);
end;

procedure TFormSamlidpserver.DoSessionCreated(Sender: TObject; ConnectionID: Int64;
  const SessionID: String);
var
  Li: TListItem;
begin
  lvSessions.Items.BeginUpdate;
  try
    Li := lvSessions.Items.Add;
    Li.Caption := SessionID;
    Li.SubItems.Add('Created');
  finally
    lvSessions.Items.EndUpdate;
  end;

  AsyncLog('Session creayed: ' + SessionID);
end;

procedure TFormSamlidpserver.DoSessionDestroyed(Sender: TObject; ConnectionID: Int64;
  const SessionID: String);
var
  Li: TListItem;
  i: integer;
begin
  lvSessions.Items.BeginUpdate;
  try
    for i := 0 to lvSessions.Items.Count - 1 do
    begin
      Li := lvSessions.Items[i];

      if SessionID = Li.Caption then
      begin
        lvSessions.Items.Delete(i);

        Break;
      end;
    end;
  finally
    lvSessions.Items.EndUpdate;
  end;

  AsyncLog('Session closed: ' + SessionID);
end;

procedure TFormSamlidpserver.DoSessionEvent(Sender: TObject; ConnectionID: Int64; const SessionID: String;
  const EventText: String);
var
  Li: TListItem;
  i: integer;
begin
  lvSessions.Items.BeginUpdate;
  try
    for i := 0 to lvSessions.Items.Count - 1 do
    begin
      Li := lvSessions.Items[i];

      if SessionID = Li.Caption then
      begin
        Li.SubItems[0] := EventText;

        Break;
      end;
    end;
  finally
    lvSessions.Items.EndUpdate;
  end;
end;

procedure TFormSamlidpserver.DoError(Sender: TObject; ConnectionID: Int64;
  const SessionID: string; ErrorCode: Integer; Fatal, Remote: boolean;
  const Description: String);
begin
  AsyncLog('Error ' + IntToStr(ErrorCode) + ': ' + Description);
end;

procedure TFormSamlidpserver.DoAuthnRequestReceived(Sender: TObject; ConnectionID: Int64;
  const SessionID: String; const RequestID: String; const SP: String; IsEncrypted: Boolean;
  IsSigned: Boolean; var NameIDFormat: String; var ForceAuthn: Boolean; var NonInteractive: Boolean;
  var Authenticated: Boolean; var Action: Integer);
begin
  AsyncLog('AuthnRequest received from ' + SP + ' in session ID ' + SessionID);
end;

procedure TFormSamlidpserver.DoUserAuthCompleted(Sender: TObject; ConnectionID: Int64;
  const SessionID: String; const SP: String; const Auth: String; const Username: String;
  var NameID: String; var NameIDFormat: String; var SessionIndex: String; var AssertionTTL: Integer);
begin
  AsyncLog('User authentication "' + Auth + '" completed for session ID ' + SessionID + ': ' + Username);
end;

procedure TFormSamlidpserver.AsyncLog(const S: string);
var
  P : PWideChar;
begin
  GetMem(P, (Length(S) + 1) * 2);
  StrPCopy(P, S);
  PostMessage(Application.MainFormHandle, WM_LOG, NativeUInt(P), 0);
end;

procedure TFormSamlidpserver.LogCapture(var Msg : TMessage);
var
  S : string;
  P : PWideChar;
begin
  P := PWideChar(Msg.WParam);
  S := StrPas(P);
  FreeMem(P);
  DoLog(S);
end;

procedure TFormSamlidpserver.FormCreate(Sender: TObject);
begin
  FServer := TsbxSAMLIdPServer.Create(nil);
  FUserMgr := TsbxUserManager.Create(nil);
end;

procedure TFormSamlidpserver.FormDestroy(Sender: TObject);
begin
  if FServer.Active then
    bbStopClick(Self);

  FreeAndNil(FServer);
  FreeAndNil(FUserMgr);
end;

procedure TFormSamlidpserver.bbAddUserClick(Sender: TObject);
var
  Li: TListItem;
begin
  if FormCredentials.ShowModal <> mrOk then
    Exit;

  Li := lvUsers.Items.Add;
  Li.Caption := FormCredentials.edLogin.Text;
  Li.SubItems.Add(FormCredentials.edPassword.Text);
  Li.SubItems.Add(FormCredentials.edEmail.Text);
end;

procedure TFormSamlidpserver.bbAutoConfigClick(Sender: TObject);
begin
  if BrowseForFolder(FAutoPath, 'Choose directory for auto generated files (should be the same for both SP and IdP samples):') then
  begin
    pnStep1.Visible := true;
    pcSteps.SelectNextPage(true);
  end;
end;

procedure TFormSamlidpserver.bbExportIDPMetadataClick(Sender: TObject);
var
  MD : TBytes;
begin
  if Length(edIDPMetadata.Text) = 0 then
  begin
    ShowMessage('You should choose output file path first!');
    Exit;
  end;

  AdjustServer;
  MD := TEncoding.UTF8.GetBytes(FServer.ExportSettings(true, -1));
  with TFileStream.Create(edIDPMetadata.Text, fmCreate) do
    try
      Write(MD[0], Length(MD));
    finally
      Free;
    end;
end;

procedure TFormSamlidpserver.bbGoto2Click(Sender: TObject);
begin
  pnStep2.Visible := true;
  pcSteps.SelectNextPage(true);
end;

procedure TFormSamlidpserver.bbGoto3Click(Sender: TObject);
begin
  edSPMetadata.Text := FAutoPath + 'sp_metadata.xml';
  sbAddSP.Click;

  edIDPMetadata.Text := FAutoPath + 'idp_metadata.xml';
  bbExportIDPMetadata.Click;

  pnStep3.Visible := true;
  pcSteps.SelectNextPage(true);
end;

procedure TFormSamlidpserver.bbGoto4Click(Sender: TObject);
begin
  mmLastStep.Visible := true;
  bbExportIDPMetadataClick(Self);
  pcSteps.SelectNextPage(true);
end;

procedure TFormSamlidpserver.bbRemoveUserClick(Sender: TObject);
begin
  if lvUsers.ItemIndex >= 0 then
    lvUsers.Items.Delete(lvUsers.ItemIndex);
end;

procedure TFormSamlidpserver.AdjustServer;
var
  Mgr : TsbxCertificateManager;
begin
  if Length(edURL.Text) = 0 then
  begin
    ShowMessage('Base URL should be set!');
    Exit;
  end;
  if Length(edMetadataURL.Text) = 0 then
  begin
    ShowMessage('Metadata URL should be set!');
    Exit;
  end;
  if Length(edSSO.Text) = 0 then
  begin
    ShowMessage('SSO URL should be set!');
    Exit;
  end;
  if Length(edSLS.Text) = 0 then
  begin
    ShowMessage('SLS URL should be set!');
    Exit;
  end;

  FServer.Port := ExtractListenPort(edURL.Text); // note: if custom port is provided, it will be adjusted in the following URL assignment
  FServer.URL := edURL.Text;
  FServer.MetadataURL := edMetadataURL.Text;
  FServer.BaseDir := FAutoPath;

  if cbSSORedirect.Checked then
    FServer.AddIdPService(spsSingleSignOnService, sbxconstants.csbtRedirect, edSSO.Text, 0, 0);
  if cbSSOPOST.Checked then
    FServer.AddIdPService(spsSingleSignOnService, sbxconstants.csbtPOST, edSSO.Text, 0, 0);
  if cbSSOArtifact.Checked then
    FServer.AddIdPService(spsSingleSignOnService, sbxconstants.csbtArtifact, edSSO.Text, 0, 0);

  if cbSLSRedirect.Checked then
    FServer.AddIdPService(spsSingleLogoutService, sbxconstants.csbtRedirect, edSLS.Text, 0, 0);
  if cbSLSPOST.Checked then
    FServer.AddIdPService(spsSingleLogoutService, sbxconstants.csbtPOST, edSLS.Text, 0, 0);
  if cbSLSArtifact.Checked then
    FServer.AddIdPService(spsSingleLogoutService, sbxconstants.csbtArtifact, edSLS.Text, 0, 0);

  FServer.SignOnPageTemplate := memoAuthForm.Text;

  Mgr := TsbxCertificateManager.Create(nil);
  try
    Mgr.OnPasswordNeeded := HandleCertMgrPasswordNeeded;
    if edSignCert.Text <> '' then
      Mgr.ImportFromFile(edSignCert.Text, '')
    else
    begin
      Mgr.CreateNew(sbxconstants.ctX509Certificate, 'generic', 'SAML IdP signing certificate');
      Mgr.Generate(2048);
    end;
    FServer.SigningCertificate := Mgr.Certificate;

    if edEncCert.Text <> '' then
      Mgr.ImportFromFile(edEncCert.Text, '')
    else
    begin
      Mgr.CreateNew(sbxconstants.ctX509Certificate, 'generic', 'SAML IdP encryption certificate');
      Mgr.Generate(2048);
    end;
    FServer.EncryptionCertificate := Mgr.Certificate;

    if edMetaSignCert.Text <> '' then
    begin
      Mgr.ImportFromFile(edMetaSignCert.Text, '');
      FServer.MetaSigningCertificate:= Mgr.Certificate;
    end
    else
      FServer.MetaSigningCertificate := FServer.SigningCertificate; // re-using the primary signing certificate

    if cbUseTLS.Checked then
    begin
      if (edServerCert.Text <> '') then
        Mgr.ImportFromFile(edServerCert.Text, '')
      else
      begin
        Mgr.CreateNew(sbxconstants.ctX509Certificate, 'tls', '127.0.0.1');
        Mgr.Generate(2048);
      end;
      FServer.TLSServerChain.Add(Mgr.Certificate);
      FServer.TLSSettings.TLSMode := smImplicitTLS;
    end;
  finally
    FreeAndNil(Mgr);
  end;
end;

procedure TFormSamlidpserver.HandleCertMgrPasswordNeeded(Sender: TObject;
  var Password: String; var Cancel: Boolean);
begin
  Password := InputBox('Password needed', 'Password needed to load the certificate. '#13#10#13#10'Note: PASSWORD NOT MASKED, TYPE WITH CARE', '');
  Cancel := false;
end;

procedure TFormSamlidpserver.bbStartClick(Sender: TObject);
var
  i, idx: integer;
  Login, Email: string;
  Buf : TBytes;
  F : TFileStream;
begin
  if lvUsers.Items.Count = 0 then
  begin
    ShowMessage('At least one user account should be registered!');
    Exit;
  end;
  if lbKnownSPs.Count = 0 then
  begin
    ShowMessage('At least one SP metadata file should be registered!');
    Exit;
  end;

  FUserMgr.Reset;
  lvUsers.Items.BeginUpdate;
  try
    for i := 0 to lvUsers.Items.Count - 1 do
    begin
      Login := lvUsers.Items[i].Caption;

      if lvUsers.Items[i].SubItems.Count > 1 then
        Email := lvUsers.Items[i].SubItems[1]
      else
        Email := '';

      idx := FUserMgr.AddUser(Login);
      FUserMgr.Users[idx].Email := Email;
      FUserMgr.Users[idx].Password := lvUsers.Items[i].SubItems[0];
    end;
  finally
    lvUsers.Items.EndUpdate;
  end;

  FServer.Users := FUserMgr.Users;

  for i := 0 to lbKnownSPs.Count - 1 do
  begin
    F := TFileStream.Create(lbKnownSPs.Items[i], fmOpenRead);
    try
      SetLength(Buf, F.Size);
      F.Read(Buf[0], Length(Buf));
    finally
      FreeAndNil(F);
    end;

    FServer.ImportSettings(TEncoding.UTF8.GetString(Buf), false);
  end;

  FServer.OnSessionCreated := DoSessionCreated;
  FServer.OnSessionDestroyed := DoSessionDestroyed;
  FServer.OnSessionEvent := DoSessionEvent;
  FServer.OnAuthnRequestReceived := DoAuthnRequestReceived;
  FServer.OnUserAuthCompleted := DoUserAuthCompleted;
  FServer.OnError := DoError;

  AdjustServer;

  FServer.OfflineMode := false;

  FServer.Start;

  bbStop.Enabled := true;
  bbStart.Enabled := false;

  mmLog.Lines.Add('Identity provider service started!');
end;

procedure TFormSamlidpserver.bbStopClick(Sender: TObject);
begin
  if FServer.Active then
  begin
    bbStop.Enabled := false;
    bbStart.Enabled := true;

    FServer.Stop;

    mmLog.Lines.Add('Identity provider service stopped!');
  end;
end;

procedure TFormSamlidpserver.sbAddSPClick(Sender: TObject);
begin
  if Length(edSPMetadata.Text) > 0 then
    lbKnownSPs.Items.Add(edSPMetadata.Text);
end;

procedure TFormSamlidpserver.sbChooseEncCertClick(Sender: TObject);
begin
  if OpenDlg.Execute then
    edEncCert.Text := OpenDlg.FileName;
end;

procedure TFormSamlidpserver.sbChooseIDPMetadataClick(Sender: TObject);
begin
  if SaveDlg.Execute then
    edIDPMetadata.Text := SaveDlg.FileName;
end;

procedure TFormSamlidpserver.sbChooseMetaSignCertClick(Sender: TObject);
begin
  if OpenDlg.Execute then
    edMetaSignCert.Text := OpenDlg.FileName;
end;

procedure TFormSamlidpserver.sbChooseServerCertClick(Sender: TObject);
begin
  if OpenDlg.Execute then
    edServerCert.Text := OpenDlg.FileName;
end;

procedure TFormSamlidpserver.sbChooseSignCertClick(Sender: TObject);
begin
  if OpenDlg.Execute then
    edSignCert.Text := OpenDlg.FileName;
end;

procedure TFormSamlidpserver.sbChooseSPMetadataClick(Sender: TObject);
begin
  if OpenDlg.Execute then
    edSPMetadata.Text := OpenDlg.FileName;
end;

end.



