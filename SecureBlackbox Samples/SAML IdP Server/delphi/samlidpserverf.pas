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
  ExtCtrls,
  SBxTypes, SBxCore, SBxSAMLIdPServer, SBxTLSServer, SBxCertificateManager;

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
    cbExternalServerMode: TCheckBox;
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
    FTLSServer: TsbxTLSServer;
    FServer: TsbxSAMLIdPServer;
    FAutoPath: string;

    procedure AdjustServer;

    procedure DoLog(const Line : string);
    procedure DoSessionEstablished(Sender: TObject; ConnectionID: Int64; const Username: String);
    procedure DoSessionClosed(Sender: TObject; ConnectionID: Int64);
    procedure DoConnect(Sender: TObject; ConnectionId: Int64; const RemoteAddress: String;
      RemotePort: Integer);
    procedure DoError(Sender: TObject; ErrorCode: Integer; const Description: String);

    procedure DoData(Sender: TObject; ConnectionID: Int64; Buffer: TBytes);

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

type
  TInteger = class
  public
    Value: Int64;
  end;

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

procedure TFormSamlidpserver.DoSessionEstablished(Sender: TObject;
  ConnectionID: Int64; const Username: String);
var
  Li: TListItem;
  I: TInteger;
begin
  lvSessions.Items.BeginUpdate;
  try
    Li := lvSessions.Items.Add;

    I := TInteger.Create;
    I.Value := ConnectionID;

    Li.Data := I;
    Li.Caption := Username;
  finally
    lvSessions.Items.EndUpdate;
  end;

  AsyncLog('Session established: ' + Username + ', connection ID: ' + IntToStr(ConnectionID));
end;

procedure TFormSamlidpserver.DoSessionClosed(Sender: TObject; ConnectionID: Int64);
var
  Li: TListItem;
  i: integer;
begin
  lvSessions.Items.BeginUpdate;
  try
    for i := 0 to lvSessions.Items.Count - 1 do
    begin
      Li := lvSessions.Items[i];

      if ConnectionID = TInteger(Li.Data).Value then
      begin
        lvSessions.Items.Delete(i);

        TInteger(Li.Data).Free;
        Li.Data := nil;

        Break;
      end;
    end;
  finally
    lvSessions.Items.EndUpdate;
  end;

  AsyncLog('Session closed: ' + IntToStr(ConnectionID));
end;

procedure TFormSamlidpserver.DoConnect(Sender: TObject; ConnectionId: Int64;
  const RemoteAddress: String; RemotePort: Integer);
begin
  AsyncLog('Client connected from ' + RemoteAddress + ':' + IntToStr(RemotePort));
end;

procedure TFormSamlidpserver.DoError(Sender: TObject; ErrorCode: Integer;
  const Description: String);
begin
  AsyncLog('Error ' + IntToStr(ErrorCode) + ': ' + Description);
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
  FTLSServer := TsbxTLSServer.Create(nil);
  FTLSServer.OnData := DoData;

  FServer := TsbxSAMLIdPServer.Create(nil);
end;

procedure TFormSamlidpserver.FormDestroy(Sender: TObject);
begin
  bbStopClick(Self);
  FreeAndNil(FServer);
  FreeAndNil(FTLSServer);
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
begin
  if Length(edIDPMetadata.Text) = 0 then
  begin
    ShowMessage('You should choose output file path first!');
    Exit;
  end;

  AdjustServer;
  FServer.SaveMetadata(edIDPMetadata.Text);
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
  S: string;
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

  FTLSServer.Port := ExtractPort(edURL.Text);
  FServer.Port := ExtractListenPort(edURL.Text); // note: if custom port is provided, it will be adjusted in the following URL assignment
  FServer.URL := edURL.Text;
  FServer.MetadataURL := edMetadataURL.Text;

  FServer.SingleSignOnService := edSSO.Text;
  FServer.SingleLogoutService := edSLS.Text;
  FServer.SingleSignOnServiceBindings := '';
  FServer.SingleLogoutServiceBindings := '';
  S := '';

  if cbSSORedirect.Checked then
    S := S + 'Redirect,';
  if cbSSOPOST.Checked then
    S := S + 'POST,';
  if cbSSOArtifact.Checked then
    S := S + 'Artifact,';

  if S[Length(S)] = ',' then
    Delete(S, Length(S), 1);

  if Length(S) = 0 then
  begin
    ShowMessage('Single SignOn Service bindings are not set!');
    Exit;
  end;

  FServer.SingleSignOnServiceBindings := S;
  S := '';

  if cbSSORedirect.Checked then
    S := S + 'Redirect,';
  if cbSSOPOST.Checked then
    S := S + 'POST,';
  if cbSSOArtifact.Checked then
    S := S + 'Artifact,';

  if S[Length(S)] = ',' then
    Delete(S, Length(S), 1);

  if Length(S) = 0 then
  begin
    ShowMessage('Single Logout Service bindings are not set!');
    Exit;
  end;

  FServer.SingleLogoutServiceBindings := S;
  FServer.AuthFormTemplate := memoAuthForm.Text;

  Mgr := TsbxCertificateManager.Create(nil);
  try
    Mgr.OnPasswordNeeded := HandleCertMgrPasswordNeeded;
    if edSignCert.Text <> '' then
      Mgr.ImportFromFile(edSignCert.Text, '')
    else
      Mgr.GetSampleCert('generic', 'SAML IdP signing certificate');
    FServer.SigningCertificate := Mgr.Certificate;

    if edEncCert.Text <> '' then
      Mgr.ImportFromFile(edEncCert.Text, '')
    else
      Mgr.GetSampleCert('generic', 'SAML IdP encryption certificate');
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
        Mgr.GetSampleCert('tls', '127.0.0.1');
      FServer.ServerCertificates.Add(Mgr.Certificate);
      //FServer.UseTLS := true;
      FServer.TLSSettings.TLSMode := smExplicitTLS;
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
  i: integer;
  Login, Email: string;
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

  FServer.ClearUsers;
  lvUsers.Items.BeginUpdate;
  try
    for i := 0 to lvUsers.Items.Count - 1 do
    begin
      Login := lvUsers.Items[i].Caption;

      if lvUsers.Items[i].SubItems.Count > 1 then
        Email := lvUsers.Items[i].SubItems[1]
      else
        Email := '';

      FServer.AddUserWithEmail(Login, Email, lvUsers.Items[i].SubItems[0]);
    end;
  finally
    lvUsers.Items.EndUpdate;
  end;

  for i := 0 to lbKnownSPs.Count - 1 do
    FServer.LoadSPMetadata(lbKnownSPs.Items[i]);

  FServer.OnSessionEstablished := DoSessionEstablished;
  FServer.OnSessionClosed := DoSessionClosed;
  FServer.OnConnect := DoConnect;
  FServer.OnError := DoError;

  AdjustServer;

  FServer.OfflineMode := cbExternalServerMode.Checked;

  if cbExternalServerMode.Checked then
    FTLSServer.Start;

  FServer.Start;

  bbStop.Enabled := true;
  bbStart.Enabled := false;

  mmLog.Lines.Add('Identity provider service started!');
end;

procedure TFormSamlidpserver.bbStopClick(Sender: TObject);
begin
  if FServer.Active or FTLSServer.Active then
  begin
    bbStop.Enabled := false;
    bbStart.Enabled := true;

    if cbExternalServerMode.Checked then
      FTLSServer.Stop
    else
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

procedure TFormSamlidpserver.DoData(Sender: TObject; ConnectionID: Int64; Buffer: TBytes);
var
  response: TBytes;
begin
  response := FServer.ProcessGenericRequest(Buffer);

  TsbxTLSServer(Sender).SendData(ConnectionID, response);
end;

end.



