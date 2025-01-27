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
unit samlspserverf;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Messages,
  Controls, Forms, Dialogs, StdCtrls, ComCtrls, Buttons, ExtCtrls,
  sbxConstants, sbxTypes, sbxCore, sbxSAMLSPServer, SBxCertificateManager;

const
  WM_LOG = WM_USER + 1;

type
  TFormSamlspserver = class(TForm)
    pcSteps: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Label16: TLabel;
    mmLog: TMemo;
    bbStart: TButton;
    bbStop: TButton;
    OpenDlg: TOpenDialog;
    TabSheet5: TTabSheet;
    Label15: TLabel;
    sbChooseProtectedRes: TSpeedButton;
    edProtectedResources: TEdit;
    Label12: TLabel;
    sbChooseIDPMetadata: TSpeedButton;
    Label13: TLabel;
    sbChooseSPMetadata: TSpeedButton;
    Label14: TLabel;
    edIDPMetadata: TEdit;
    edSPMetadata: TEdit;
    bbExportSPMetadata: TBitBtn;
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
    Label4: TLabel;
    Label5: TLabel;
    edURL: TEdit;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label6: TLabel;
    edACS: TEdit;
    cbACSRedirect: TCheckBox;
    cbACSPOST: TCheckBox;
    cbACSArtifact: TCheckBox;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label7: TLabel;
    edSLS: TEdit;
    cbSLSRedirect: TCheckBox;
    cbSLSPOST: TCheckBox;
    cbSLSArtifact: TCheckBox;
    edLogoutPage: TEdit;
    edMetadataURL: TEdit;
    mmStep1: TMemo;
    bbAutoConfig: TButton;
    pnStep1: TPanel;
    Label18: TLabel;
    bbGoto2: TButton;
    pnStep2: TPanel;
    Label19: TLabel;
    bbGoto3: TButton;
    pnStep3: TPanel;
    Label20: TLabel;
    bbGoto4: TButton;
    mmLastStep: TMemo;
    Memo1: TMemo;
    Panel2: TPanel;
    Label21: TLabel;
    Label22: TLabel;
    procedure bbStartClick(Sender: TObject);
    procedure bbStopClick(Sender: TObject);
    procedure bbExportSPMetadataClick(Sender: TObject);
    procedure sbChooseSignCertClick(Sender: TObject);
    procedure sbChooseEncCertClick(Sender: TObject);
    procedure sbChooseMetaSignCertClick(Sender: TObject);
    procedure sbChooseServerCertClick(Sender: TObject);
    procedure sbChooseIDPMetadataClick(Sender: TObject);
    procedure sbChooseSPMetadataClick(Sender: TObject);
    procedure sbChooseProtectedResClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bbAutoConfigClick(Sender: TObject);
    procedure bbGoto2Click(Sender: TObject);
    procedure bbGoto3Click(Sender: TObject);
    procedure bbGoto4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FServer: TsbxSAMLSPServer;
    FAutoPath: string;

    function AdjustServer: boolean;
    function ExtractHTMLFile(const Name: string): string;
    procedure DoLog(const Line : string);
    procedure DoLogAsync(const Line : string);
    procedure LogCapture(var Msg : TMessage); message WM_LOG;

    procedure HandleCertMgrPasswordNeeded(Sender: TObject;
      var Password: String; var Cancel: Boolean);
    procedure HandleServerError(Sender: TObject; ConnectionID: Int64;
      const SessionID: String; ErrorCode: Integer; Fatal: Boolean;
      Remote: Boolean; const Description: String);
    procedure HandleUserAuthCompleted(Sender: TObject; ConnectionID: Int64;
      const SessionID: String; const AssertionBody: String; const Auth: String;
      const NameID: String; const NameIDFormat: String; const SessionIndex: String;
      const ValidFrom: String; const ValidTo: String; const RelayState: String);
  public
    { Public declarations }
  end;

var
  FormSamlspserver: TFormSamlspserver;

implementation

uses
  SHLObj;

{$R *.dfm}

const
  SampleHTML = '<HTML><HEAD><TITLE>SecureBlackbox SAML server demo</TITLE></HEAD>'+
    '<BODY><CENTER><H1>SecureBlackbox SAML demo works!</H1><a href="%LOGOUT%">Logout</a></CENTER></BODY></HTML>';

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
    raise Exception.Create('Unsupported base URL!');
end;

function ExtractPort(const URL: string): integer;
var
  Idx: integer;
begin
  Idx := LastDelimiter(':', URL);
  Result := StrToIntDef(URL.Substring(Idx), 80);
end;

procedure TFormSamlspserver.DoLog(const Line : string);
begin
  mmLog.Lines.Add(Line);
end;

procedure TFormSamlspserver.DoLogAsync(const Line : string);
var
  P : PWideChar;
begin
  GetMem(P, (Length(Line) + 1) * 2);
  StrPCopy(P, Line);
  PostMessage(Application.MainFormHandle, WM_LOG, NativeUInt(P), 0);
end;

procedure TFormSamlspserver.LogCapture(var Msg : TMessage);
var
  S : string;
  P : PWideChar;
begin
  P := PWideChar(Msg.WParam);
  S := StrPas(P);
  FreeMem(P);
  DoLog(S);
end;

procedure TFormSamlspserver.FormCreate(Sender: TObject);
begin
  FServer := TsbxSAMLSPServer.Create(nil);
end;

procedure TFormSamlspserver.FormDestroy(Sender: TObject);
begin
  bbStopClick(Self);
  FreeAndNil(FServer);
end;

procedure TFormSamlspserver.bbAutoConfigClick(Sender: TObject);
begin
  if BrowseForFolder(FAutoPath, 'Choose directory for auto generated files (should be the same for both SP and IdP samples):') then
  begin
    pnStep1.Visible := true;
    pcSteps.SelectNextPage(true);
  end;
end;

procedure TFormSamlspserver.bbExportSPMetadataClick(Sender: TObject);
var
  Metadata : TBytes;
  F : TFileStream;
begin
  if Length(edSPMetadata.Text) = 0 then
  begin
    ShowMessage('Please provide the metadata file location');
    Exit;
  end;

  if AdjustServer then
  begin
    Metadata := TEncoding.UTF8.GetBytes(FServer.ExportSettings(true));

    F := TFileStream.Create(edSPMetadata.Text, fmCreate);
    try
      F.Write(Metadata[0], Length(Metadata));
    finally
      FreeAndNil(F);
    end;
  end
  else
    MessageDlg('Failed to export metadata. Please correct the errors and try again.', mtError, [mbOk], 0);
end;

function TFormSamlspserver.ExtractHTMLFile(const Name: string): string;
var
  FS: TFileStream;
  S: AnsiString;
begin
  Result := FAutoPath + Name;

  if FileExists(Result) then
    DeleteFile(Result);

  S := StringReplace(SampleHTML, '%LOGOUT%', edLogoutPage.Text, [rfReplaceAll]);

  FS := TFileStream.Create(Result, fmCreate or fmOpenReadWrite);
  try
    FS.WriteBuffer(S[1], Length(S));
  finally
    FreeAndNil(FS);
  end;
end;

procedure TFormSamlspserver.bbGoto2Click(Sender: TObject);
begin
  pnStep2.Visible := true;
  pcSteps.SelectNextPage(true);
end;

procedure TFormSamlspserver.bbGoto3Click(Sender: TObject);
begin
  edIDPMetadata.Text := FAutoPath + 'idp_metadata.xml';
  edSPMetadata.Text := FAutoPath + 'sp_metadata.xml';

  pnStep3.Visible := true;
  pcSteps.SelectNextPage(true);
end;

procedure TFormSamlspserver.bbGoto4Click(Sender: TObject);
begin
  edProtectedResources.Text := FAutoPath;
  ExtractHTMLFile('index.html');

  mmLastStep.Lines.Text := StringReplace(mmLastStep.Lines.Text, '%URL%',
    edURL.Text + '/index.html', [rfReplaceAll]);

  mmLastStep.Visible := true;
  pcSteps.SelectNextPage(true);
end;

function TFormSamlspserver.AdjustServer: boolean;
var
  Metadata: TBytes;
  F : TFileStream;
  Mgr : TsbxCertificateManager;
begin
  Result := false;

  if Length(edURL.Text) = 0 then
  begin
    ShowMessage('The base URL should be set!');
    Exit;
  end;
  if Length(edLogoutPage.Text) = 0 then
  begin
    ShowMessage('The logout URL should be set!');
    Exit;
  end;
  if Length(edMetadataURL.Text) = 0 then
  begin
    ShowMessage('The metadata URL should be set!');
    Exit;
  end;
  if Length(edACS.Text) = 0 then
  begin
    ShowMessage('ACS URL should be set!');
    Exit;
  end;
  if Length(edSLS.Text) = 0 then
  begin
    ShowMessage('SLS URL should be set!');
    Exit;
  end;
  if Length(edProtectedResources.Text) = 0 then
  begin
    ShowMessage('The resources path should be set!');
    Exit;
  end;
  if Length(edIDPMetadata.Text) = 0 then
  begin
    ShowMessage('IdP metadata file path should be set!');
    Exit;
  end;

  F := TFileStream.Create(edIDPMetadata.Text, fmOpenRead);
  try
    SetLength(Metadata, F.Size);
    F.Read(Metadata[0], Length(Metadata));
  finally
    FreeAndNil(F);
  end;

  FServer.ImportSettings(TEncoding.UTF8.GetString(Metadata), false);

  FServer.Port := ExtractListenPort(edURL.Text);
  FServer.URL := edURL.Text;
  FServer.LogoutPage := edLogoutPage.Text;
  FServer.MetadataURL := edMetadataURL.Text;
  FServer.BaseDir := edProtectedResources.Text;
  FServer.OfflineMode := false;

  if not cbACSRedirect.Checked and not cbACSPOST.Checked and not cbACSArtifact.Checked then
  begin
    ShowMessage('Please choose Assertion Consumer Service bindings!');
    Exit;
  end;

  if cbACSRedirect.Checked then
    FServer.AddSPService(spsAssertionConsumerService, Integer(csbtRedirect), edACS.Text, -1, -1);
  if cbACSPOST.Checked then
    FServer.AddSPService(spsAssertionConsumerService, Integer(csbtPost), edACS.Text, -1, -1);
  if cbACSArtifact.Checked then
    FServer.AddSPService(spsAssertionConsumerService, Integer(csbtArtifact), edACS.Text, -1, -1);

  if not cbSLSRedirect.Checked and not cbSLSPOST.Checked and not cbSLSArtifact.Checked then
  begin
    ShowMessage('Please choose Single Logout Service bindings!');
    Exit;
  end;

  if cbSLSRedirect.Checked then
    FServer.AddSPService(spsSingleLogoutService, Integer(csbtRedirect), edSLS.Text, -1, -1);
  if cbSLSPOST.Checked then
    FServer.AddSPService(spsSingleLogoutService, Integer(csbtPost), edSLS.Text, -1, -1);
  if cbSLSArtifact.Checked then
    FServer.AddSPService(spsSingleLogoutService, Integer(csbtArtifact), edSLS.Text, -1, -1);

  Mgr := TsbxCertificateManager.Create(nil);
  try
    Mgr.OnPasswordNeeded := HandleCertMgrPasswordNeeded;

    if edSignCert.Text <> '' then
      Mgr.ImportFromFile(edSignCert.Text, '')
    else
    begin
      Mgr.CreateNew(SBxConstants.ctX509Certificate, 'generic', 'SAML SP signing certificate');
      Mgr.Generate(2048);
    end;
    FServer.SigningCertificate := Mgr.Certificate;

    if edEncCert.Text <> '' then
      Mgr.ImportFromFile(edEncCert.Text, '')
    else
    begin
      Mgr.CreateNew(SBxConstants.ctX509Certificate, 'generic', 'SAML SP encryption certificate');
      Mgr.Generate(2048);
    end;
    FServer.DecryptionCertificate := Mgr.Certificate;

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
        Mgr.CreateNew(SBxConstants.ctX509Certificate, 'tls', '127.0.0.1');
        Mgr.Generate(2048);
      end;

      FServer.TLSServerChain.Add(Mgr.Certificate);
      FServer.TLSSettings.TLSMode := smExplicitTLS;
    end;
  finally
    FreeAndNil(Mgr);
  end;

  FServer.RedirectOnLogoutPage := 'https://www.nsoftware.com';
  FServer.OnError := HandleServerError;
  FServer.OnUserAuthCompleted := HandleUserAuthCompleted;

  Result := true;
end;

procedure TFormSamlspserver.HandleCertMgrPasswordNeeded(Sender: TObject;
  var Password: String; var Cancel: Boolean);
begin
  Password := InputBox('Password needed', 'Password needed to load the certificate. '#13#10#13#10'Note: PASSWORD NOT MASKED, TYPE WITH CARE', '');
  Cancel := false;
end;

procedure TFormSamlspserver.HandleServerError(Sender: TObject; ConnectionID: Int64;
  const SessionID: String; ErrorCode: Integer; Fatal: Boolean;
  Remote: Boolean; const Description: String);
begin
  DoLogAsync('Server error ' + IntToStr(ErrorCode) + ' in session ' + SessionID + ': ' + Description);
end;

procedure TFormSamlspserver.HandleUserAuthCompleted(Sender: TObject; ConnectionID: Int64;
  const SessionID: String; const AssertionBody: String; const Auth: String;
  const NameID: String; const NameIDFormat: String; const SessionIndex: String;
  const ValidFrom: String; const ValidTo: String; const RelayState: String);
begin
  DoLogAsync('User authenticated in session ' + SessionID + ': ' + NameID + ' / ' + SessionIndex);
end;

procedure TFormSamlspserver.bbStartClick(Sender: TObject);
begin
  if not AdjustServer then
  begin
    MessageDlg('Could not start the server due to configuration errors. Please correct the errors and try again.', mtError, [mbOk], 0);
    Exit;
  end;

  FServer.Start;

  bbStart.Enabled := false;
  bbStop.Enabled := true;
  DoLog('Service provider started!');
end;

procedure TFormSamlspserver.bbStopClick(Sender: TObject);
begin
  if FServer.Active then
  begin
    FServer.Stop;

    bbStart.Enabled := true;
    bbStop.Enabled := false;
    DoLog('Service provider stopped!');
  end;
end;

procedure TFormSamlspserver.sbChooseEncCertClick(Sender: TObject);
begin
  if OpenDlg.Execute then
    edEncCert.Text := OpenDlg.FileName;
end;

procedure TFormSamlspserver.sbChooseIDPMetadataClick(Sender: TObject);
begin
  if OpenDlg.Execute then
    edIDPMetadata.Text := OpenDlg.FileName;
end;

procedure TFormSamlspserver.sbChooseMetaSignCertClick(Sender: TObject);
begin
  if OpenDlg.Execute then
    edMetaSignCert.Text := OpenDlg.FileName;
end;

procedure TFormSamlspserver.sbChooseProtectedResClick(Sender: TObject);
var
  Folder: string;
begin
  if BrowseForFolder(Folder, 'Choose Folder') then
    edProtectedResources.Text := Folder;
end;

procedure TFormSamlspserver.sbChooseServerCertClick(Sender: TObject);
begin
  if OpenDlg.Execute then
    edServerCert.Text := OpenDlg.FileName;
end;

procedure TFormSamlspserver.sbChooseSignCertClick(Sender: TObject);
begin
  if OpenDlg.Execute then
    edSignCert.Text := OpenDlg.FileName;
end;

procedure TFormSamlspserver.sbChooseSPMetadataClick(Sender: TObject);
begin
  if OpenDlg.Execute then
    edSPMetadata.Text := OpenDlg.FileName;
end;

end.


