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
unit websocketserverf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  SBxTypes, SBxCertificateManager, SBxWebSocketServer, Winapi.ShellAPI;

type
  TFormWebsocketserver = class(TForm)
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
    mmLog: TMemo;
    OpenFileDlg: TOpenDialog;
    Label10: TLabel;
    procedure SpeedButton1Click(Sender: TObject);
    procedure bbStartClick(Sender: TObject);
    procedure bbStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FServer: TsbxWebSocketServer;

    function ReadFileAsString(const FileName: string): string;

    procedure DoAccept(Sender: TObject; const RemoteAddress: String;
      RemotePort: Integer; var Accept: Boolean);
    procedure DoGet(Sender: TObject; ConnectionId: Int64;
      const URI: String; var Handled: Boolean);
    procedure DoTextData(Sender: TObject; ConnectionID: Int64; const Text: String; Last: Boolean);
  public
    { Public declarations }
  end;

var
  FormWebsocketserver: TFormWebsocketserver;

implementation

{$R *.dfm}

function TFormWebsocketserver.ReadFileAsString(const FileName: string): string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(FileName);
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure TFormWebsocketserver.DoAccept(Sender: TObject; const RemoteAddress: String;
  RemotePort: Integer; var Accept: Boolean);
begin
  mmLog.Lines.Add('New connection from ' + RemoteAddress);
  Accept := true;
end;

procedure TFormWebsocketserver.DoGet(Sender: TObject; ConnectionId: Int64;
  const URI: String; var Handled: Boolean);
var
  Content, ContentType: string;
begin
  Handled := true;

  if (URI = '/') or (URI = '/index.html') then
  begin
    Content := ReadFileAsString('index.html');
    ContentType := 'text/html';
  end
  else if URI = '/client.js' then
  begin
    Content := ReadFileAsString('client.js');
    ContentType := 'text/javascript';
  end
  else
  begin
    Content := '404 Not Found';
    ContentType := 'text/plain';
    FServer.SetResponseStatus(ConnectionId, 404);
  end;

  FServer.SetResponseString(ConnectionId, Content, ContentType, '');
end;

procedure TFormWebsocketserver.DoTextData(Sender: TObject; ConnectionID: Int64; const Text: String; Last: Boolean);
begin
  // send data back
  FServer.SendText(ConnectionID, Text);
end;

procedure TFormWebsocketserver.bbStartClick(Sender: TObject);
var
  Port: integer;
  CertificateManager: TsbxCertificateManager;
begin
  if FServer.Active then
    Exit;

  if TryStrToInt(edPort.Text, Port) then
    FServer.Port := Port;

  if cbUseTLS.Checked then
  begin
    FServer.TLSSettings.TLSMode := TsbxWebSocketServerTLSTLSModes.smImplicitTLS;
    CertificateManager := TsbxCertificateManager.Create(nil);
    try
      try
        CertificateManager.ImportFromFile(edCertFile.Text, edCertPassword.Text);

        FServer.ServerCertificates.Add(CertificateManager.Certificate);
      except
        on E: Exception do
          MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
      end;
    finally
      FreeAndNil(CertificateManager);
    end;
  end;

  FServer.OnAccept := DoAccept;
  FServer.OnGetRequest := DoGet;
  FServer.OnTextData := DoTextData;

  FServer.Start;

  if cbUseTLS.Checked then
    ShellExecute(0, 'open', PChar('https://localhost:' + edPort.Text), nil, nil, SW_SHOWNORMAL)
  else
    ShellExecute(0, 'open', PChar('http://localhost:' + edPort.Text), nil, nil, SW_SHOWNORMAL);
end;

procedure TFormWebsocketserver.bbStopClick(Sender: TObject);
begin
  if FServer.Active then
    FServer.Stop;
end;

procedure TFormWebsocketserver.FormCreate(Sender: TObject);
begin
  FServer := TsbxWebSocketServer.Create(nil);
end;

procedure TFormWebsocketserver.FormDestroy(Sender: TObject);
begin
  if Assigned(FServer) then
    FServer.Free;
end;

procedure TFormWebsocketserver.SpeedButton1Click(Sender: TObject);
begin
  if OpenFileDlg.Execute then
    edCertFile.Text := OpenFileDlg.FileName;
end;

end.




