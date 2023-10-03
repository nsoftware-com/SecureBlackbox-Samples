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
unit tlsclientf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  SBxTypes, SBxTLSClient;

type
  TFormTlsclient = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edAddress: TEdit;
    Label2: TLabel;
    edPort: TEdit;
    btnDisconnect: TButton;
    Label3: TLabel;
    edMsg: TEdit;
    mmMsg: TMemo;
    btnConnect: TButton;
    btnSend: TButton;
    Label10: TLabel;
    cbUseTLS: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
  private
    { Private declarations }
    FClient: TsbxTLSClient;

    procedure DoCertValidate(Sender: TObject; const ServerHost: String; const ServerIP : string;
  var Accept: Boolean);
  public
    { Public declarations }
  end;

var
  FormTlsclient: TFormTlsclient;

implementation

{$R *.dfm}

procedure TFormTlsclient.DoCertValidate(Sender: TObject; const ServerHost: String; const ServerIP : string;
  var Accept: Boolean);
begin
  Accept := true;
end;

procedure TFormTlsclient.btnConnectClick(Sender: TObject);
var
  Port: integer;
begin
  try
    Port := StrToInt(edPort.Text);
  except
    ShowMessage('Wrong port value!');
    Exit;
  end;

  if cbUseTLS.Checked then
    FClient.TLSSettings.TLSMode := smDefault
  else
    FClient.TLSSettings.TLSMode := smNoTLS;

  FClient.Connect(edAddress.Text, Port);

  btnConnect.Enabled := false;
  btnDisconnect.Enabled := true;
  btnSend.Enabled := true;
end;

procedure TFormTlsclient.btnDisconnectClick(Sender: TObject);
begin
  FClient.Disconnect;

  btnConnect.Enabled := true;
  btnDisconnect.Enabled := false;
  btnSend.Enabled := false;
end;

procedure TFormTlsclient.btnSendClick(Sender: TObject);
begin
  FClient.SendText(edMsg.Text);
  mmMsg.Lines.Add('C->S: ' + edMsg.Text);

  FClient.ReceiveAllData(Length(edMsg.Text));
  mmMsg.Lines.Add('S->C: ' + FClient.OutputString);
end;

procedure TFormTlsclient.FormCreate(Sender: TObject);
begin
  FClient := TsbxTLSClient.Create(nil);
  FClient.OnTLSCertValidate := DoCertValidate;
  FClient.TLSSettings.AutoValidateCertificates := false;
end;

procedure TFormTlsclient.FormDestroy(Sender: TObject);
begin
  if FClient.Connected then
    FClient.Disconnect;

  FreeAndNil(FClient);
end;

end.



