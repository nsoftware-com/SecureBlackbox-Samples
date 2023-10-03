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
unit dtlsserverf;

interface

uses
  Windows, Messages, SysUtils, StrUtils, Classes, Controls, Forms, Dialogs, ComCtrls, StdCtrls,
  sbxDTLSServer, sbxCertificateManager;

const
  WM_LOG = WM_USER + 1;

type
  TFormDtlsServer = class(TForm)
    lblInfo: TLabel;
    grpSettings: TGroupBox;
    lblPort: TLabel;
    edtPort: TEdit;
    udPort: TUpDown;
    btnStart: TButton;
    grpLog: TGroupBox;
    lvwLog: TListView;
    btnStop: TButton;
    lblCertificate: TLabel;
    edtFilename: TEdit;
    btnBrowse: TButton;
    dlgOpenCertificate: TOpenDialog;
    lblPassword: TLabel;
    edtPassword: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
  private
    FServer: TsbxDTLSServer;
    FStarted: TDateTime;
    FStartTicks: Cardinal;
    procedure DoAccept(Sender: TObject; const RemoteAddress: string; RemotePort: Integer; var Accept: Boolean);
    procedure DoData(Sender: TObject; ConnectionID: Int64; Buffer: TBytes);
    procedure DoError(Sender: TObject; ConnectionID: Int64; ErrorCode: Integer; const Description: string);
    procedure DoTLSEstablished(Sender: TObject; ConnectionID: Int64);
    procedure DoTLSShutdown(Sender: TObject; ConnectionID: Int64);
    procedure WMLog(var Msg: TMessage); message WM_LOG;
  public
    procedure Log(const Text: string);
  end;

var
  FormDtlsServer: TFormDtlsServer;

implementation

{$R *.dfm}

procedure TFormDtlsServer.btnBrowseClick(Sender: TObject);
begin
  if dlgOpenCertificate.Execute(Self.Handle) then
    edtFilename.Text := dlgOpenCertificate.Filename;
end;

procedure TFormDtlsServer.btnStartClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
begin
  if FServer.Active then
  begin
    Log('Server already started');
    Exit;
  end;

  if edtFilename.Text = '' then
  begin
    MessageDlg('A server certificate is required for this demo.', mtError, [mbOk], 0);
    Exit;
  end;

  FServer.ServerCertificates.Clear();
  CertificateManager := TsbxCertificateManager.Create(nil);
  try
    try
      CertificateManager.ImportFromFile(edtFilename.Text, edtPassword.Text);
      FServer.ServerCertificates.Add(CertificateManager.Certificate);
      Log('Server certificate loaded');
    except
      on E: Exception do
      begin
        MessageDlg('Failed to load the server certificate.'#13#10 + E.Message, mtError, [mbOK], 0);
        Exit;
      end;
    end;
  finally
    FreeAndNil(CertificateManager);
  end;

  FServer.Port := StrToInt(edtPort.Text);

  FServer.Start();
  Log('Server started');
end;

procedure TFormDtlsServer.btnStopClick(Sender: TObject);
begin
  if not FServer.Active then
  begin
    Log('Server not started');
    Exit;
  end;

  FServer.Stop();
  Log('Server stopped');
end;

procedure TFormDtlsServer.DoAccept(Sender: TObject; const RemoteAddress: string; RemotePort: Integer;
  var Accept: Boolean);
begin
  Accept := True;
  Log('Accepted a new client from ' + RemoteAddress + ':' + IntToStr(RemotePort));
end;

procedure TFormDtlsServer.DoData(Sender: TObject; ConnectionID: Int64; Buffer: TBytes);
var
  T: AnsiString;
  S: string;
begin
  SetLength(T, Length(Buffer));
  Move(Buffer[0], T[1], Length(T));
  S := string(T);
  Log(Format('[%d] C->S: %s', [ConnectionID, S]));

  // reverse the received text and send it back to the client
  S := ReverseString(S);
  FServer.SendText(ConnectionID, S);
  Log(Format('[%d] S->C: %s', [ConnectionID, S]));
end;

procedure TFormDtlsServer.DoError(Sender: TObject; ConnectionID: Int64; ErrorCode: Integer; const Description: string);
begin
  Log(Format('[%d] Error %d: %s', [ConnectionID, ErrorCode, Description]));
end;

procedure TFormDtlsServer.DoTLSEstablished(Sender: TObject; ConnectionID: Int64);
begin
  Log(Format('[%d] Secure session established', [ConnectionID]));
end;

procedure TFormDtlsServer.DoTLSShutdown(Sender: TObject; ConnectionID: Int64);
begin
  Log(Format('[%d] Secure session closed', [ConnectionID]));
end;

procedure TFormDtlsServer.FormCreate(Sender: TObject);
begin
  FStarted := Now();
  FStartTicks := GetTickCount();

  FServer := TsbxDTLSServer.Create(nil);
  FServer.OnAccept := DoAccept;
  FServer.OnData := DoData;
  FServer.OnError := DoError;
  FServer.OnTLSEstablished := DoTLSEstablished;
  FServer.OnTLSShutdown := DoTLSShutdown;
end;

procedure TFormDtlsServer.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FServer);
end;

procedure TFormDtlsServer.Log(const Text: string);
var
  P: PChar;
  T: Cardinal;
begin
  // as this method could be called from different threads,
  // it's needed to post a message to the main thread
  P := StrNew(PChar(Text));
  T := GetTickCount() - FStartTicks;
  PostMessage(Self.Handle, WM_LOG, WParam(T), LParam(P));
end;

procedure TFormDtlsServer.WMLog(var Msg: TMessage);
var
  P: PChar;
  S: string;
  Item: TListItem;
  T: Cardinal;
begin
  T := Cardinal(Msg.WParam);

  P := PChar(Msg.LParam);
  S := StrPas(P);
  StrDispose(P);

  Item := lvwLog.Items.Add();
  Item.Caption := TimeToStr(FStarted + T / 86400000);
  Item.SubItems.Add(S);
end;

end.

