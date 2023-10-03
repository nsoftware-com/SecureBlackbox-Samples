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
unit smtpclientf;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls, IniFiles,
  sbxtypes, SBxMailWriter, SBxSMTPClient, XPMan;

type
  TFormSmtpclient = class(TForm)
    lblInfo: TLabel;
    grpServer: TGroupBox;
    grpMessage: TGroupBox;
    lblHost: TLabel;
    edtHost: TEdit;
    lblPort: TLabel;
    edtPort: TEdit;
    udPort: TUpDown;
    lblSecurity: TLabel;
    rbtNoTLS: TRadioButton;
    rbtExplicitTLS: TRadioButton;
    rbtImplicitTLS: TRadioButton;
    lblLogin: TLabel;
    edtLogin: TEdit;
    lblPassword: TLabel;
    edtPassword: TEdit;
    btnSend: TButton;
    lblFrom: TLabel;
    edtFromName: TEdit;
    edtFromAddress: TEdit;
    lblNameHint: TLabel;
    lblAddressHint: TLabel;
    lblSubject: TLabel;
    lblTo: TLabel;
    edtToName: TEdit;
    edtToAddress: TEdit;
    edtSubject: TEdit;
    lblText: TLabel;
    lblPriority: TLabel;
    rbtLowest: TRadioButton;
    rbtLow: TRadioButton;
    rbtNormal: TRadioButton;
    rbtHigh: TRadioButton;
    rbtHighest: TRadioButton;
    memText: TMemo;
    procedure rbtNoTLSClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FLog: TFileStream;
    procedure LoadSettings();
    procedure LogProtocolCommand(Sender: TObject; const Cmd: string);
    procedure LogProtocolData(Sender: TObject; const Cmd, Data: string);
    procedure LogProtocolReply(Sender: TObject; const Cmd, Reply: string);
    procedure LogString(const S: string);
    procedure SaveSettings();
  public
    { Public declarations }
  end;

var
  FormSmtpclient: TFormSmtpclient;

implementation

{$R *.dfm}

const
  IniSection = 'SMTP';

procedure TFormSmtpclient.btnSendClick(Sender: TObject);
var
  Writer: TsbxMailWriter;
  Client: TsbxSMTPClient;
  Addr: TsbxMailAddress;
  Buffer: TBytes;
begin
  SaveSettings();
  try
    Client := TsbxSMTPClient.Create(nil);
    try
      Client.OnCommand := LogProtocolCommand;
      Client.OnCommandData := LogProtocolData;
      Client.OnCommandReply := LogProtocolReply;

      // prepare the message

      Writer := TsbxMailWriter.Create(nil);
      try
        Addr := TsbxMailAddress.Create(edtFromName.Text, edtFromAddress.Text);
        Writer.From.Add(Addr);
        FreeAndNil(Addr);

        Addr := TsbxMailAddress.Create(edtToName.Text, edtToAddress.Text);
        Writer.SendTo.Add(Addr);
        FreeAndNil(Addr);

        Writer.Message.Subject := edtSubject.Text;

        if rbtLowest.Checked then
          Writer.Message.Priority := mpLowest
        else
        if rbtLow.Checked then
          Writer.Message.Priority := mpLow
        else
        if rbtHigh.Checked then
          Writer.Message.Priority := mpHigh
        else
        if rbtHighest.Checked then
          Writer.Message.Priority := mpHighest;

        Writer.Message.PlainText := memText.Lines.Text;

        // if a message is big enough, it's better to use SaveToStream/SendStream or 
        // SaveToFile/SendFile methods instead of SaveToBytes/SendBytes
        Buffer := Writer.SaveToBytes();
      finally
        FreeAndNil(Writer);
      end;

      // configure the client

      if rbtExplicitTLS.Checked then
        Client.TLSSettings.TLSMode := smExplicitTLS
      else
      if rbtImplicitTLS.Checked then
        Client.TLSSettings.TLSMode := smImplicitTLS;

      Client.Username := edtLogin.Text;
      Client.Password := edtPassword.Text;

      Client.Connect(edtHost.Text, StrToInt(edtPort.Text));
      try
        Client.SendBytes(Buffer);
      finally
        Client.Disconnect();
      end;
    finally
      FreeAndNil(Client);
    end;

    MessageDlg('Message has been sent successfully', mtInformation, [mbOk], 0);
  except
    on E: Exception do
      MessageDlg('Failed to send the message:'#13#10 + E.Message, mtError, [mbOk], 0);
  end;
end;

procedure TFormSmtpclient.FormCreate(Sender: TObject);
begin
  udPort.Max := 65535;
  udPort.Position := 25;

  LoadSettings();
  FLog := TFileStream.Create(ChangeFileExt(Application.ExeName, '.log'), fmCreate or fmShareDenyWrite);
end;

procedure TFormSmtpclient.FormDestroy(Sender: TObject);
begin
  SaveSettings();
  FreeAndNil(FLog);
end;

procedure TFormSmtpclient.LoadSettings();
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

procedure TFormSmtpclient.LogProtocolCommand(Sender: TObject; const Cmd: string);
begin
  LogString(Cmd);
end;

procedure TFormSmtpclient.LogProtocolData(Sender: TObject; const Cmd, Data: string);
begin
  LogString('[data: ' + IntToStr(Length(Data)) + ' bytes]');
end;

procedure TFormSmtpclient.LogProtocolReply(Sender: TObject; const Cmd, Reply: string);
begin
  LogString(Reply);
end;

procedure TFormSmtpclient.LogString(const S: string);
var
  Buffer: AnsiString;
begin
  if FLog = nil then
    Exit;

  Buffer := AnsiString(S + #13#10);
  FLog.WriteBuffer(Buffer[1], Length(Buffer));
end;

procedure TFormSmtpclient.rbtNoTLSClick(Sender: TObject);
var
  Port: Integer;
begin
  Port := StrToIntDef(edtPort.Text, 0);
  if rbtNoTLS.Checked then
  begin
    if (Port = 465) or (Port = 587) then
      udPort.Position := 25;
  end
  else
  if rbtExplicitTLS.Checked then
  begin
    if (Port = 25) or (Port = 465) then
      udPort.Position := 587;
  end
  else
  if rbtImplicitTLS.Checked then
  begin
    if (Port = 25) or (Port = 587) then
      udPort.Position := 465;
  end;
end;

procedure TFormSmtpclient.SaveSettings();
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

end.



