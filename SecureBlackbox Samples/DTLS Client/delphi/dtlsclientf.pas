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
unit dtlsclientf;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls, sbxdtlsclient;

type
  TFormDtlsClient = class(TForm)
    lblInfo: TLabel;
    grpServer: TGroupBox;
    lblHost: TLabel;
    grpMessage: TGroupBox;
    edtHost: TEdit;
    lblPort: TLabel;
    edtPort: TEdit;
    udPort: TUpDown;
    btnConnect: TButton;
    lblText: TLabel;
    memText: TMemo;
    btnSend: TButton;
    grpLog: TGroupBox;
    lvwAllMessages: TListView;
    btnDisconnect: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
  private
    FClient: TsbxDTLSClient;
    FTimer: TTimer;
    procedure DoCertificateValidate(Sender: TObject; const ServerHost: string; const ServerIP: string; var Accept: Boolean);
    procedure DoTimer(Sender: TObject);
    procedure Log(const Text: string);
  public
    { Public declarations }
  end;

var
  FormDtlsClient: TFormDtlsClient;

implementation

{$R *.dfm}

procedure TFormDtlsClient.btnConnectClick(Sender: TObject);
begin
  try
    FClient.Connect(edtHost.Text, StrToInt(edtPort.Text));
    Log('Secure session established');

    btnConnect.Enabled := False;
    btnDisconnect.Enabled := True;
    btnSend.Enabled := True;
    FTimer.Enabled := True;
  except
    on E: Exception do
      MessageDlg('Failed to establish a secure session.'#13#10 + E.Message, mtError, [mbOk], 0);
  end;
end;

procedure TFormDtlsClient.btnDisconnectClick(Sender: TObject);
begin
  FTimer.Enabled := False;

  FClient.Disconnect();
  Log('Secure session closed');

  btnConnect.Enabled := True;
  btnDisconnect.Enabled := False;
  btnSend.Enabled := False;
end;

procedure TFormDtlsClient.btnSendClick(Sender: TObject);
var
  S: string;
begin
  S := memText.Lines.Text;
  FClient.SendText(S);
  Log('C->S: ' + S);
end;

procedure TFormDtlsClient.DoCertificateValidate(Sender: TObject; const ServerHost: string; const ServerIP: string; var Accept: Boolean);
begin
  if FClient.TLSServerChain.Count = 0 then
    Exit;
  // do not do this in production code
  Accept := True;
  Log('Server certificate received: ' + FClient.TLSServerChain[0].Subject);
end;

procedure TFormDtlsClient.DoTimer(Sender: TObject);
var
  S: string;
begin
  if FClient.Connected then
  begin
    // check if there is some incoming data from the server
    FClient.ReceiveData(10240);
    S := FClient.OutputString;
    if Length(S) <> 0 then
      Log('S->C: ' + S);
  end
  else
  begin
    // the session was closed by the server
    FTimer.Enabled := False;
    Log('Secure session closed');
    btnConnect.Enabled := True;
    btnDisconnect.Enabled := False;
    btnSend.Enabled := False;
  end;
end;

procedure TFormDtlsClient.FormCreate(Sender: TObject);
begin
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := DoTimer;
  FTimer.Interval := 500;

  FClient := TsbxDTLSClient.Create(nil);
  FClient.SocketSettings.Timeout := 10000;
  // do not do this in production code
  FClient.TLSSettings.AutoValidateCertificates := False;
  FClient.OnTLSCertValidate := DoCertificateValidate;
end;

procedure TFormDtlsClient.Log(const Text: string);
var
  Item: TListItem;
begin
  Item := lvwAllMessages.Items.Add();
  Item.Caption := FormatDateTime('hh:nn:ss', Now());
  Item.SubItems.Add(Text);
end;

end.
