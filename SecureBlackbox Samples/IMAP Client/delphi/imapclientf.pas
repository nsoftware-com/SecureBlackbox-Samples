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
unit imapclientf;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls, IniFiles,
  sbxtypes, sbximapclient;

type
  TFormImapClient = class(TForm)
    lblInfo: TLabel;
    grpServer: TGroupBox;
    lblHost: TLabel;
    lblPort: TLabel;
    lblSecurity: TLabel;
    lblLogin: TLabel;
    lblPassword: TLabel;
    edtHost: TEdit;
    edtPort: TEdit;
    udPort: TUpDown;
    rbtNoTLS: TRadioButton;
    rbtExplicitTLS: TRadioButton;
    rbtImplicitTLS: TRadioButton;
    edtLogin: TEdit;
    edtPassword: TEdit;
    btnConnect: TButton;
    btnDisconnect: TButton;
    grpMailboxes: TGroupBox;
    grpCurrentMailbox: TGroupBox;
    btnExamine: TButton;
    btnSelect: TButton;
    lvwMailboxes: TListView;
    lblNameTitle: TLabel;
    lblName: TLabel;
    lblTotalTitle: TLabel;
    lblUnseenTitle: TLabel;
    lblRecentTitle: TLabel;
    lblTotal: TLabel;
    lblUnseen: TLabel;
    lblRecent: TLabel;
    btnCreate: TButton;
    btnDelete: TButton;
    btnRefresh: TButton;
    grpMessages: TGroupBox;
    btnListAll: TButton;
    btnListNew: TButton;
    btnListRecent: TButton;
    btnListUnseen: TButton;
    btnListDeleted: TButton;
    lvwMessages: TListView;
    lblReadOnly: TLabel;
    btnMarkDeleted: TButton;
    btnPurge: TButton;
    lblFilename: TLabel;
    edtFilename: TEdit;
    btnReceive: TButton;
    btnPost: TButton;
    procedure rbtNoTLSClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnExamineClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure btnListAllClick(Sender: TObject);
    procedure btnListNewClick(Sender: TObject);
    procedure btnListRecentClick(Sender: TObject);
    procedure btnListUnseenClick(Sender: TObject);
    procedure btnListDeletedClick(Sender: TObject);
    procedure btnMarkDeletedClick(Sender: TObject);
    procedure btnPurgeClick(Sender: TObject);
    procedure btnReceiveClick(Sender: TObject);
    procedure btnPostClick(Sender: TObject);
  private
    FClient: TsbxIMAPClient;
    FLog: TFileStream;
    function IsConnected(): Boolean;
    procedure LoadSettings();
    procedure SaveSettings();
    procedure OpenLog();
    procedure CloseLog();
    procedure LogString(const S: string);
    procedure LogCommand(Sender: TObject; const Cmd: string);
    procedure LogCommandData(Sender: TObject; const Cmd, Data: string);
    procedure LogCommandReply(Sender: TObject; const Cmd, Reply: string);
    procedure LogCommandReplyData(Sender: TObject; const Cmd, Data: string);
    procedure ClearCurrentMailbox();
    procedure LoadCurrentMailbox();
    procedure ClearMailboxes();
    procedure LoadMailboxes();
    procedure ClearMessages();
    procedure LoadMessages();
    procedure SelectMailbox(const Name: string);
  public
    { Public declarations }
  end;

var
  FormImapClient: TFormImapClient;

implementation

{$R *.dfm}

const
  IniSection = 'IMAP';

{ TFormImapClient }

procedure TFormImapClient.btnConnectClick(Sender: TObject);
begin
  if FClient <> nil then
  begin
    MessageDlg('Already connected. Please disconnect first.', mtWarning, [mbOk], 0);
    Exit;
  end;

  FClient := TsbxIMAPClient.Create(nil);
  try
    FClient.OnCommand := LogCommand;
    FClient.OnCommandData := LogCommandData;
    FClient.OnCommandReply := LogCommandReply;
    FClient.OnCommandReplyData := LogCommandReplyData;

    // for demo purposes only
    FClient.TLSSettings.RevocationCheck := crcAnyCRL;

    if rbtExplicitTLS.Checked then
      FClient.TLSSettings.TLSMode := smExplicitTLS
    else
    if rbtImplicitTLS.Checked then
      FClient.TLSSettings.TLSMode := smImplicitTLS;

    FClient.Username := edtLogin.Text;
    FClient.Password := edtPassword.Text;

    FClient.Connect(edtHost.Text, StrToInt(edtPort.Text));
  except
    on E: Exception do
    begin
      MessageDlg('Failed to connect to the server.'#13#10 + E.Message, mtError, [mbOk], 0);
      FreeAndNil(FClient);
      Exit;
    end;
  end;

  MessageDlg('Connected to the server. Listing mailboxes...', mtInformation, [mbOk], 0);
  try
    FClient.ListMailboxes();

    if FClient.Mailboxes.Count = 0 then
      MessageDlg('No mailboxes on the server.', mtInformation, [mbOk], 0)
    else
      LoadMailboxes();
  except
    on E: Exception do
    begin
      MessageDlg('Failed to list mailboxes on the server.'#13#10 + E.Message, mtError, [mbOk], 0);
      try
        FClient.Disconnect();
      except
      end;
      FreeAndNil(FClient);
    end;
  end;
end;

procedure TFormImapClient.btnCreateClick(Sender: TObject);
var
  Mailbox: string;
begin
  if not IsConnected() then
    Exit;

  if lvwMailboxes.Selected = nil then
    Mailbox := ''
  else
    Mailbox := lvwMailboxes.Selected.Caption;

  if not InputQuery(Application.MainForm.Caption, 'New mailbox name:', Mailbox) then
    Exit;

  try
    FClient.CreateMailbox(Mailbox);
  except
    on E: Exception do
    begin
      MessageDlg('Failed to create a mailbox.'#13#10 + E.Message, mtError, [mbOk], 0);
      Exit;
    end;
  end;

  LoadMailboxes();
  SelectMailbox(Mailbox);
end;

procedure TFormImapClient.btnDeleteClick(Sender: TObject);
var
  Index: Integer;
begin
  if not IsConnected() then
    Exit;

  if lvwMailboxes.Selected = nil then
  begin
    MessageDlg('No mailbox selected in the list', mtWarning, [mbOk], 0);
    Exit;
  end;

  Index := lvwMailboxes.Selected.Index;

  try
    FClient.DeleteMailbox(lvwMailboxes.Selected.Caption);
  except
    on E: Exception do
    begin
      MessageDlg('Failed to delete the mailbox.'#13#10'Error: ' + E.Message, mtError, [mbOk], 0);
      Exit;
    end;
  end;

  LoadMailboxes();
  if Index >= lvwMailboxes.Items.Count then
    Index := lvwMailboxes.Items.Count - 1;
  lvwMailboxes.Items[Index].Selected := True;
  lvwMailboxes.Selected.MakeVisible(False);

  if FClient.CurrentMailbox = nil then
  begin
    ClearCurrentMailbox();
    ClearMessages();
  end;
end;

procedure TFormImapClient.btnDisconnectClick(Sender: TObject);
begin
  if FClient = nil then
    Exit;

  try
    FClient.Disconnect();
  except
  end;
  FreeAndNil(FClient);
  ClearMessages();
  ClearCurrentMailbox();
  ClearMailboxes();
end;

procedure TFormImapClient.btnExamineClick(Sender: TObject);
var
  Mailbox: string;
begin
  if not IsConnected() then
    Exit;

  if lvwMailboxes.Selected = nil then
  begin
    MessageDlg('No mailbox selected in the list', mtWarning, [mbOk], 0);
    Exit;
  end;

  Mailbox := lvwMailboxes.Selected.Caption;

  try
    FClient.ExamineMailbox(Mailbox);
  except
    on E: Exception do
    begin
      MessageDlg('Failed to examine the mailbox.'#13#10'Error: ' + E.Message, mtError, [mbOk], 0);
      Exit;
    end;
  end;

  ClearMessages();
  LoadCurrentMailbox();
end;

procedure TFormImapClient.btnListAllClick(Sender: TObject);
begin
  if not IsConnected() then
    Exit;

  try
    FClient.ListAllMessages();
  except
    on E: Exception do
    begin
      MessageDlg('Failed to list messages.'#13#10 + E.Message, mtError, [mbOk], 0);
      Exit;
    end;
  end;

  LoadMessages();
end;

procedure TFormImapClient.btnListDeletedClick(Sender: TObject);
begin
  if not IsConnected() then
    Exit;

  try
    FClient.ListDeletedMessages();
  except
    on E: Exception do
    begin
      MessageDlg('Failed to list deleted messages.'#13#10 + E.Message, mtError, [mbOk], 0);
      Exit;
    end;
  end;

  LoadMessages();
end;

procedure TFormImapClient.btnListNewClick(Sender: TObject);
begin
  if not IsConnected() then
    Exit;

  try
    FClient.ListNewMessages();
  except
    on E: Exception do
    begin
      MessageDlg('Failed to list new messages.'#13#10 + E.Message, mtError, [mbOk], 0);
      Exit;
    end;
  end;

  LoadMessages();
end;

procedure TFormImapClient.btnListRecentClick(Sender: TObject);
begin
  if not IsConnected() then
    Exit;

  try
    FClient.ListRecentMessages();
  except
    on E: Exception do
    begin
      MessageDlg('Failed to list recent messages.'#13#10 + E.Message, mtError, [mbOk], 0);
      Exit;
    end;
  end;

  LoadMessages();
end;

procedure TFormImapClient.btnListUnseenClick(Sender: TObject);
begin
  if not IsConnected() then
    Exit;

  try
    FClient.ListUnseenMessages();
  except
    on E: Exception do
    begin
      MessageDlg('Failed to list unseen messages.'#13#10 + E.Message, mtError, [mbOk], 0);
      Exit;
    end;
  end;

  LoadMessages();
end;

procedure TFormImapClient.btnMarkDeletedClick(Sender: TObject);
begin
  if not IsConnected() then
    Exit;

  if lvwMessages.Selected = nil then
  begin
    MessageDlg('No message selected in the list.', mtWarning, [mbOk], 0);
    Exit;
  end;

  try
    FClient.MarkMessageDeleted(FClient.Messages[lvwMessages.Selected.Index].UID);
  except
    on E: Exception do
    begin
      MessageDlg('Failed to mark the message deleted.'#13#10 + E.Message, mtError, [mbOk], 0);
      Exit;
    end;
  end;

  LoadCurrentMailbox();
  MessageDlg('The has been marked deleted.', mtInformation, [mbOk], 0);
end;

procedure TFormImapClient.btnPostClick(Sender: TObject);
begin
  if not IsConnected() then
    Exit;

  if edtFilename.Text = '' then
  begin
    MessageDlg('No filename specified.', mtWarning, [mbOk], 0);
    Exit;
  end;

  try
    FClient.PostFile(edtFilename.Text, 0, '');
  except
    on E: Exception do
    begin
      MessageDlg('Failed to post the message.'#13#10 + E.Message, mtError, [mbOk], 0);
      Exit;
    end;
  end;

  LoadMessages();
  MessageDlg('The message has been posted successfully.', mtInformation, [mbOk], 0);
end;

procedure TFormImapClient.btnPurgeClick(Sender: TObject);
begin
  if not IsConnected() then
    Exit;

  try
    FClient.PurgeMessages();
  except
    on E: Exception do
    begin
      MessageDlg('Failed to purge deleted messages.'#13#10 + E.Message, mtError, [mbOk], 0);
      Exit;
    end;
  end;

  LoadCurrentMailbox();
  LoadMessages();
end;

procedure TFormImapClient.btnSelectClick(Sender: TObject);
var
  Mailbox: string;
begin
  if not IsConnected() then
    Exit;

  if lvwMailboxes.Selected = nil then
  begin
    MessageDlg('No mailbox selected in the list', mtWarning, [mbOk], 0);
    Exit;
  end;

  Mailbox := lvwMailboxes.Selected.Caption;

  try
    FClient.SelectMailbox(Mailbox);
  except
    on E: Exception do
    begin
      MessageDlg('Failed to select the mailbox.'#13#10 + E.Message, mtError, [mbOk], 0);
      Exit;
    end;
  end;

  ClearMessages();
  LoadCurrentMailbox();
end;

procedure TFormImapClient.btnReceiveClick(Sender: TObject);
begin
  if not IsConnected() then
    Exit;

  if lvwMessages.Selected = nil then
  begin
    MessageDlg('No message selected in the list.', mtWarning, [mbOk], 0);
    Exit;
  end;

  if edtFilename.Text = '' then
  begin
    MessageDlg('No filename specified.', mtWarning, [mbOk], 0);
    Exit;
  end;

  try
    FClient.ReceiveFile(FClient.Messages[lvwMessages.Selected.Index].UID, edtFilename.Text);
  except
    on E: Exception do
    begin
      MessageDlg('Failed to receive the message.'#13#10 + E.Message, mtError, [mbOk], 0);
      Exit;
    end;
  end;

  MessageDlg('The message has been received successfully.', mtInformation, [mbOk], 0);
end;

procedure TFormImapClient.btnRefreshClick(Sender: TObject);
begin
  LoadCurrentMailbox();
end;

procedure TFormImapClient.ClearCurrentMailbox();
begin
  lblName.Caption := '[no current mailbox]';
  lblTotal.Caption := '-';
  lblUnseen.Caption := '-';
  lblRecent.Caption := '-';
  lblReadOnly.Caption := '';
end;

procedure TFormImapClient.ClearMailboxes();
begin
  lvwMailboxes.Items.Clear();
end;

procedure TFormImapClient.ClearMessages();
begin
  lvwMessages.Items.Clear();
end;

procedure TFormImapClient.CloseLog();
begin
  FreeAndNil(FLog);
end;

procedure TFormImapClient.FormCreate(Sender: TObject);
begin
  udPort.Max := 65535;
  udPort.Position := 143;

  LoadSettings();
  OpenLog();
end;

procedure TFormImapClient.FormDestroy(Sender: TObject);
begin
  CloseLog();
  SaveSettings();
end;

function TFormImapClient.IsConnected(): Boolean;
begin
  Result := (FClient <> nil);
  if not Result then
    MessageDlg('Not connected. Please connect to an IMAP4 server first.', mtWarning, [mbOk], 0);
end;

procedure TFormImapClient.LogCommand(Sender: TObject; const Cmd: string);
begin
  LogString('C: ' + Cmd);
end;

procedure TFormImapClient.LogCommandData(Sender: TObject; const Cmd, Data: string);
begin
  LogString('C: [data: ' + IntToStr(Length(Data)) + ' bytes]');
end;

procedure TFormImapClient.LogCommandReply(Sender: TObject; const Cmd, Reply: string);
begin
  LogString('S: ' + Reply);
end;

procedure TFormImapClient.LogCommandReplyData(Sender: TObject; const Cmd, Data: string);
begin
  LogString('S: [data: ' + IntToStr(Length(Data)) + ' bytes]');
end;

procedure TFormImapClient.LoadCurrentMailbox();
begin
  if not IsConnected() then
    Exit;

  if FClient.CurrentMailbox = nil then
    ClearCurrentMailbox()
  else
  begin
    lblName.Caption := FClient.CurrentMailbox.Name;
    lblTotal.Caption := IntToStr(FClient.CurrentMailbox.TotalMessages);
    lblUnseen.Caption := IntToStr(FClient.CurrentMailbox.UnseenMessages);
    lblRecent.Caption := IntToStr(FClient.CurrentMailbox.RecentMessages);
    if FClient.CurrentMailbox.ReadOnly then
      lblReadOnly.Caption := 'Read-Only'
    else
      lblReadOnly.Caption := '';
  end;
end;

procedure TFormImapClient.LoadMailboxes();
var
  I: Integer;
  Info: TsbxIMAPMailboxInfo;
  Item: TListItem;
begin
  lvwMailboxes.Items.BeginUpdate();
  try
    lvwMailboxes.Items.Clear();

    for I := 0 to FClient.Mailboxes.Count - 1 do
    begin
      Info := FClient.Mailboxes[I];

      Item := lvwMailboxes.Items.Add();
      Item.Caption := Info.Name;

      if Info.HasChildren then
        Item.SubItems.Add('+')
      else
      if Info.HasNoChildren then
        Item.SubItems.Add('-')
      else
        Item.SubItems.Add('');

      if Info.Marked then
        Item.SubItems.Add('+')
      else
      if Info.Unmarked then
        Item.SubItems.Add('-')
      else
        Item.SubItems.Add('');

      if Info.NoSelect then
        Item.SubItems.Add('-')
      else
        Item.SubItems.Add('');

      if Info.NoInferiors then
        Item.SubItems.Add('-')
      else
        Item.SubItems.Add('');
    end;
  finally
    lvwMailboxes.Items.EndUpdate();
  end;
end;

procedure TFormImapClient.LoadMessages();
var
  I: Integer;
  Info: TsbxIMAPMessageInfo;
  Item: TListItem;
begin
  lvwMessages.Items.BeginUpdate();
  try
    lvwMessages.Items.Clear();

    for I := 0 to FClient.Messages.Count - 1 do
    begin
      Info := FClient.Messages[I];

      Item := lvwMessages.Items.Add();
      Item.Caption := Info.From;
      Item.SubItems.Add(Info.SentTo);
      Item.SubItems.Add(Info.Subject);
      Item.SubItems.Add(Info.Date);
      Item.SubItems.Add(IntToStr(Info.Size));
    end;
  finally
    lvwMessages.Items.EndUpdate();
  end;
end;

procedure TFormImapClient.LoadSettings();
var
  Ini: TIniFile;
  S: string;
begin
  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    edtHost.Text := Ini.ReadString(IniSection, 'Host', edtHost.Text);
    udPort.Position := Ini.ReadInteger(IniSection, 'Port', udPort.Position);
    S := Ini.ReadString(IniSection, 'SSL', 'none');
    if SameText(S, 'explicit') then
      rbtExplicitTLS.Checked := True
    else
    if SameText(S, 'implicit') then
      rbtImplicitTLS.Checked := True;
    rbtNoTLSClick(nil);
    edtLogin.Text := Ini.ReadString(IniSection, 'Login', edtLogin.Text);
    edtPassword.Text := Ini.ReadString(IniSection, 'Password', edtPassword.Text);
  finally
    FreeAndNil(Ini);
  end;
end;

procedure TFormImapClient.LogString(const S: string);
var
  Buffer: AnsiString;
begin
  if FLog = nil then
    Exit;

  Buffer := AnsiString(S + #13#10);
  FLog.WriteBuffer(Buffer[1], Length(Buffer));
end;

procedure TFormImapClient.OpenLog();
begin
  FLog := TFileStream.Create(ChangeFileExt(Application.ExeName, '.log'), fmCreate or fmShareDenyWrite);
end;

procedure TFormImapClient.rbtNoTLSClick(Sender: TObject);
var
  Port: Integer;
begin
  Port := StrToIntDef(edtPort.Text, 0);
  if rbtNoTLS.Checked or rbtExplicitTLS.Checked then
  begin
    if Port = 993 then
      udPort.Position := 143;
  end
  else
  if rbtImplicitTLS.Checked then
  begin
    if Port = 143 then
      udPort.Position := 993;
  end;
end;

procedure TFormImapClient.SaveSettings();
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    Ini.WriteString(IniSection, 'Host', edtHost.Text);
    Ini.WriteInteger(IniSection, 'Port', udPort.Position);
    if rbtExplicitTLS.Checked then
      Ini.WriteString(IniSection, 'SSL', 'explicit')
    else
    if rbtImplicitTLS.Checked then
      Ini.WriteString(IniSection, 'SSL', 'implicit')
    else
      Ini.WriteString(IniSection, 'SSL', 'none');
    Ini.WriteString(IniSection, 'Login', edtLogin.Text);
    Ini.WriteString(IniSection, 'Password', edtPassword.Text);
  finally
    FreeAndNil(Ini);
  end;
end;

procedure TFormImapClient.SelectMailbox(const Name: string);
var
  I: Integer;
begin
  for I := 0 to lvwMailboxes.Items.Count - 1 do
    if lvwMailboxes.Items[I].Caption = Name then
    begin
      lvwMailboxes.Items[I].Selected := True;
      lvwMailboxes.Selected.MakeVisible(False);
      Exit;
    end;
end;

end.

