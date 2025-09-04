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
unit popclientf;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls, XPMan,
  sbxtypes, SBxPOP3Client;

type
  TFormPopclient = class(TForm)
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
    grpList: TGroupBox;
    btnConnect: TButton;
    btnDisconnect: TButton;
    lvwMessages: TListView;
    btnDelete: TButton;
    btnReceive: TButton;
    grpMessage: TGroupBox;
    lblFrom: TLabel;
    edtFrom: TEdit;
    lblTo: TLabel;
    edtTo: TEdit;
    lblSubject: TLabel;
    lblDate: TLabel;
    edtDate: TEdit;
    lblPriority: TLabel;
    edtPriority: TEdit;
    edtSubject: TEdit;
    Label1: TLabel;
    memTextPlain: TMemo;
    Label2: TLabel;
    memTextHtml: TMemo;
    lblAttachments: TLabel;
    lblAttachCount: TLabel;
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure rbtNoTLSClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnReceiveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FClient: TsbxPOP3Client;
    procedure ClearMessage();
    procedure LoadMessages();
  public
    { Public declarations }
  end;

var
  FormPopclient: TFormPopclient;

implementation

{$R *.dfm}

procedure TFormPopclient.btnConnectClick(Sender: TObject);
begin
  if FClient <> nil then
  begin
    MessageDlg('Already connected. Please disconnect first.', mtWarning, [mbOk], 0);
    Exit;
  end;

  FClient := TsbxPOP3Client.Create(nil);
  try
    FClient.Config('RequestUIDs=True');

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
      try
        FClient.Disconnect();
      except
      end;
      FreeAndNil(FClient);
      exit;
    end;
  end;

  MessageDlg('Connected to the server. Listing messages...', mtInformation, [mbOk], 0);
  try
    FClient.ListMessages();

    if FClient.Messages.Count = 0 then
      MessageDlg('No messages on the server.', mtInformation, [mbOk], 0)
    else
    begin
      LoadMessages();
      MessageDlg('There is(are) ' + IntToStr(FClient.Messages.Count) + ' message(s) on the server.', mtInformation, [mbOk], 0);
    end;
  except
    on E: Exception do
    begin
      MessageDlg('Failed to list messages on the server.'#13#10 + E.Message, mtError, [mbOk], 0);
      try
        FClient.Disconnect();
      except
      end;
      FreeAndNil(FClient);
    end;
  end;
end;

procedure TFormPopclient.btnDeleteClick(Sender: TObject);
begin
  if FClient = nil then
  begin
    MessageDlg('Not connected. Please connect to a POP3 server first.', mtWarning, [mbOk], 0);
    Exit;
  end;

  if lvwMessages.Selected = nil then
  begin
    MessageDlg('Please select a message to delete.', mtWarning, [mbOk], 0);
    Exit;
  end;

  if MessageDlg('Are you sure you want to delete the message "' + FClient.Messages[lvwMessages.Selected.Index].UID + '"?',
                mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    Exit;

  try
    FClient.DeleteMessage(lvwMessages.Selected.Index);
    LoadMessages();
  except
    on E: Exception do
      MessageDlg('Failed to delete the message.'#13#10 + E.Message, mtError, [mbOk], 0);
  end;
end;

procedure TFormPopclient.btnDisconnectClick(Sender: TObject);
begin
  if FClient = nil then
    Exit;

  lvwMessages.Items.Clear();

  try
    FClient.Disconnect();
  except
  end;
  FreeAndNil(FClient);
end;

procedure TFormPopclient.btnReceiveClick(Sender: TObject);
begin
  if FClient = nil then
  begin
    MessageDlg('Not connected. Please connect to a POP3 server first.', mtWarning, [mbOk], 0);
    Exit;
  end;

  if lvwMessages.Selected = nil then
  begin
    MessageDlg('Please select a message to receive.', mtWarning, [mbOk], 0);
    Exit;
  end;

  ClearMessage();
  try
    FClient.ReceiveMessage(lvwMessages.Selected.Index);
  except
    on E: Exception do
    begin
      MessageDlg('Failed to receive the message', mtError, [mbOk], 0);
      Exit;
    end;
  end;

  edtFrom.Text := FClient.Message.From;
  edtTo.Text := FClient.Message.SendTo;
  edtDate.Text := FClient.Message.Date;
  edtSubject.Text := FClient.Message.Subject;
  case FClient.Message.Priority of
    mpLowest:
      edtPriority.Text := '[lowest]';
    mpLow:
      edtPriority.Text := '[low]';
    mpNormal:
      edtPriority.Text := '';
    mpHigh:
      edtPriority.Text := '[HIGH]';
    mpHighest:
      edtPriority.Text := '[HIGHEST]';
  end;

  memTextPlain.Lines.Text := FClient.Message.PlainText;
  memTextHtml.Lines.Text := FClient.Message.HtmlText;

  if FClient.Message.AttachmentCount = 0 then
    lblAttachCount.Caption := '[none]'
  else
    lblAttachCount.Caption := IntToStr(FClient.Message.AttachmentCount);
end;

procedure TFormPopclient.ClearMessage();
begin
  edtFrom.Text := '';
  edtTo.Text := '';
  edtDate.Text := '';
  edtPriority.Text := '';
  edtSubject.Text := '';
  memTextPlain.Lines.Clear();
  memTextHtml.Lines.Clear();
  lblAttachCount.Caption := '[none]';
end;

procedure TFormPopclient.FormCreate(Sender: TObject);
begin
  udPort.Max := 65535;
  udPort.Position := 110;
end;

procedure TFormPopclient.LoadMessages();
var
  I: Integer;
  Item: TListItem;
begin
  lvwMessages.Items.BeginUpdate();
  try
    lvwMessages.Items.Clear();
    for I := 0 to FClient.Messages.Count - 1 do
    begin
      Item := lvwMessages.Items.Add();
      Item.Caption := FClient.Messages[I].UID;
      Item.SubItems.Add(IntToStr(FClient.Messages[I].Size));
    end;
  finally
    lvwMessages.Items.EndUpdate();
  end;
end;

procedure TFormPopclient.rbtNoTLSClick(Sender: TObject);
var
  Port: Integer;
begin
  Port := StrToIntDef(edtPort.Text, 0);
  if rbtNoTLS.Checked or rbtExplicitTLS.Checked then
  begin
    if Port = 995 then
      udPort.Position := 110;
  end
  else
  if rbtImplicitTLS.Checked then
  begin
    if Port = 110 then
      udPort.Position := 995;
  end;
end;

end.


