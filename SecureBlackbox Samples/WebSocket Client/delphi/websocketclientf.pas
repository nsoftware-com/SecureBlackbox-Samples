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
unit websocketclientf;

{$ifdef UNICODE}
  {$define SB_UNICODE_VCL}
{$endif}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  sbxtypes, sbxcore, sbxWebsocketClient;

type
  TFormWebsocketclient = class(TForm)
    btnConnect: TButton;
    MMLog: TMemo;
    dlgSave: TSaveDialog;
    Label10: TLabel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EdHost: TEdit;
    CbProtocol: TComboBox;
    EdPort: TEdit;
    editText: TEdit;
    btnSend: TButton;
    btnDisconnect: TButton;
    procedure btnConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
  private
    { Private declarations }
    WsClient: TsbxWebsocketClient;
    procedure HandleTextData(Sender: TObject; const Text: String; Last: Boolean);
    procedure HandleTLSCertValidate(Sender: TObject; const ServerHost: String;
      const ServerIP: String; var Accept: Boolean);
  public
  end;

var
  FormWebsocketclient: TFormWebsocketclient;

implementation

{$R *.DFM}

procedure TFormWebsocketclient.btnConnectClick(Sender: TObject);
begin
  try
    btnConnect.Enabled := false;
    btnDisconnect.Enabled := true;

    WsClient.Connect(CbProtocol.Text + '://' + EdHost.Text + ':' + EdPort.Text, '');

    MMLog.Lines.Add('Connected');
  except
    on E : Exception do
    begin
      try
        WsClient.Disconnect;
      except
        ;
      end;

      ShowMessage('Connection failed: ' + E.Message);

      btnConnect.Enabled := true;
      btnDisconnect.Enabled := false;
    end;
  end;
end;

procedure TFormWebsocketclient.FormCreate(Sender: TObject);
begin
  CbProtocol.ItemIndex := 0;
  EdHost.Text := '127.0.0.1';
  EdPort.Text := '8080';

  WsClient := TsbxWebsocketClient.Create(nil);
  WsClient.OnTextData := HandleTextData;
  WsClient.TLSSettings.AutoValidateCertificates := false;
  WsClient.OnTLSCertValidate := HandleTLSCertValidate;
end;

procedure TFormWebsocketclient.FormDestroy(Sender: TObject);
begin
  FreeAndNil(WsClient);
end;

procedure TFormWebsocketclient.btnDisconnectClick(Sender: TObject);
begin
  try
    if WsClient.Connected then
    begin
      try
        WsClient.Disconnect;
      except
        ;
      end;
    end;
  finally
    btnConnect.Enabled := true;
    btnDisconnect.Enabled := false;
  end;
end;

procedure TFormWebsocketclient.btnSendClick(Sender: TObject);
begin
  if not WsClient.Connected then
  begin
    ShowMessage('The client is not connected - connect first');
    Exit;
  end;

  MMLog.Lines.Add('< ' + editText.Text);

  WsClient.SendText(editText.Text);
  editText.Text := '';
end;

procedure TFormWebsocketclient.HandleTextData(Sender: TObject; const Text: String;
  Last: Boolean);
begin
  MMLog.Lines.Add('> ' + Text);
end;

procedure TFormWebsocketclient.HandleTLSCertValidate(Sender: TObject; const ServerHost: String;
  const ServerIP: String; var Accept: Boolean);
begin
  MMLog.Lines.Add('TLS certificate received - allowing (insecure!)');
  Accept := true; // don't do that in real-world applications!
end;

end.

