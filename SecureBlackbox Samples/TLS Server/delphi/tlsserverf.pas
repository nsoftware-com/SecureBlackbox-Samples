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
unit tlsserverf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, StrUtils, SBxCertificateManager, SBxTLSServer, SBxTypes;

type
  TFormTlsserver = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    SpeedButton1: TSpeedButton;
    Label4: TLabel;
    edPort: TEdit;
    cbUseTLS: TCheckBox;
    edCertFile: TEdit;
    edCertPassword: TEdit;
    bbStart: TButton;
    bbStop: TButton;
    Label5: TLabel;
    Label10: TLabel;
    mLog: TMemo;
    OpenDialog: TOpenDialog;
    procedure bbStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bbStopClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    Server: TsbxTLSServer;

    procedure DoAccept(Sender: TObject; const RemoteAddress: String;
      RemotePort: Integer; var Accept: Boolean);
    procedure DoTLSEstablished(Sender: TObject; ConnectionID: Int64);
    procedure OnTLSShutdown(Sender: TObject; ConnectionID: Int64);
    procedure DoData(Sender: TObject; ConnectionID: Int64; const Buffer: TBytes);
  public
    { Public declarations }
  end;

var
  FormTlsserver: TFormTlsserver;

implementation

{$R *.dfm}

procedure TFormTlsserver.bbStopClick(Sender: TObject);
begin
  if Server.Active then
    Server.Stop;
end;

procedure TFormTlsserver.DoAccept(Sender: TObject; const RemoteAddress: String;
  RemotePort: Integer; var Accept: Boolean);
begin
  mLog.Lines.Add('Accepted connection from ' + RemoteAddress);
end;

procedure TFormTlsserver.DoTLSEstablished(Sender: TObject; ConnectionID: Int64);
begin
  mLog.Lines.Add('[' + IntToStr(ConnectionID) + '] Established TLS session');
end;

procedure TFormTlsserver.OnTLSShutdown(Sender: TObject; ConnectionID: Int64);
begin
  mLog.Lines.Add('[' + IntToStr(ConnectionID) + '] Shutdown TLS session');
end;

procedure TFormTlsserver.SpeedButton1Click(Sender: TObject);
begin
  OpenDialog.Title := 'Select certificate file';
  OpenDialog.Filter := 'Certificates (*.pem, *.cer, *.crt, *.der, *.pfx, *.p12, *.pkcs12)|*.pem;*.cer;*.crt;*.der;*.pfx;*.p12;*.pkcs12|PEM-encoded certificates (*.pem)|*.pem|' +
    'DER-encoded certificates (*.cer, *.crt, *.der)|*.cer;*.crt;*.der|PKCS#12 encoded certificates (*.pfx, *.p12, *.pkcs12)|*.pfx;*.p12;*.pkcs12|All files (*.*)|*.*';
  if OpenDialog.Execute then
    edCertFile.Text := OpenDialog.FileName;
end;

procedure TFormTlsserver.DoData(Sender: TObject; ConnectionID: Int64; const Buffer: TBytes);
var
  S: string;
begin
  S := StringOf(Buffer);
  mLog.Lines.Add('[' + IntToStr(ConnectionID) + '] Received data: ' + S);

  S := ReverseString(S);
  TsbxTLSServer(Sender).SendText(ConnectionID, S);
  mLog.Lines.Add('[' + IntToStr(ConnectionID) + '] Sending data back: ' + S);
end;

procedure TFormTlsserver.bbStartClick(Sender: TObject);
var
  Port: integer;
  CertificateManager: TsbxCertificateManager;
begin
  if cbUseTLS.Checked then
    Server.TLSSettings.TLSMode := smImplicitTLS
  else
    Server.TLSSettings.TLSMode := smNoTLS;

  if cbUseTLS.Checked then
  begin
    CertificateManager := TsbxCertificateManager.Create(nil);
    try
      try
        CertificateManager.ImportFromFile(edCertFile.Text, edCertPassword.Text);

        Server.TLSServerChain.Add(CertificateManager.Certificate);
      except
        on E: Exception do
          MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
      end;
    finally
      FreeAndNil(CertificateManager);
    end;
  end;

  if TryStrToInt(edPort.Text, Port) then
    Server.Port := Port;

  mLog.Lines.Clear;

  Server.Start;
end;

procedure TFormTlsserver.FormCreate(Sender: TObject);
begin
  Server := TsbxTLSServer.Create(nil);

  Server.OnData := DoData;
  Server.OnAccept := DoAccept;
  Server.OnTLSEstablished := DoTLSEstablished;
  Server.OnTLSShutdown := OnTLSShutdown;
end;

procedure TFormTlsserver.FormDestroy(Sender: TObject);
begin
  if Server.Active then
    Server.Stop;
  FreeAndNil(Server);
end;

end.
