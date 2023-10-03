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
unit httpserverf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  SBxTypes, SBxCertificateManager, SBxHTTPServer;

type
  TFormHttpserver = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edPort: TEdit;
    cbUseTLS: TCheckBox;
    Label2: TLabel;
    edFileDir: TEdit;
    sbChooseFileDir: TSpeedButton;
    Label3: TLabel;
    edCertFile: TEdit;
    SpeedButton1: TSpeedButton;
    bbStart: TButton;
    bbStop: TButton;
    mmLog: TMemo;
    FolderOpenDlg: TFileOpenDialog;
    OpenFileDlg: TOpenDialog;
    Label4: TLabel;
    edCertPassword: TEdit;
    Label10: TLabel;
    Label5: TLabel;
    procedure sbChooseFileDirClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bbStartClick(Sender: TObject);
    procedure bbStopClick(Sender: TObject);
  private
    { Private declarations }
    FServer: TsbxHTTPServer;

    procedure DoAccept(Sender: TObject; const RemoteAddress: String;
      RemotePort: Integer; var Accept: Boolean);
  public
    { Public declarations }
  end;

var
  FormHttpserver: TFormHttpserver;

implementation

{$R *.dfm}

procedure TFormHttpserver.DoAccept(Sender: TObject; const RemoteAddress: String;
  RemotePort: Integer; var Accept: Boolean);
begin
  mmLog.Lines.Add('New connection from ' + RemoteAddress);
  Accept := true;
end;

procedure TFormHttpserver.bbStartClick(Sender: TObject);
var
  Port: integer;
  CertificateManager: TsbxCertificateManager;
  FileDir: string;
begin
  if FServer.Active then
    Exit;

  if Length(edFileDir.Text) = 0 then
    FileDir := GetCurrentDir
  else
    FileDir := edFileDir.Text;

  FServer.DocumentRoot := FileDir;

  if TryStrToInt(edPort.Text, Port) then
    FServer.Port := Port;

  if cbUseTLS.Checked then
    FServer.TLSSettings.TLSMode := smImplicitTLS
  else
    FServer.TLSSettings.TLSMode := smNoTLS;

  if cbUseTLS.Checked then
  begin
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
  FServer.Start;
end;

procedure TFormHttpserver.bbStopClick(Sender: TObject);
begin
  if FServer.Active then
    FServer.Stop;
end;

procedure TFormHttpserver.FormCreate(Sender: TObject);
begin
  FServer := TsbxHTTPServer.Create(nil);
end;

procedure TFormHttpserver.FormDestroy(Sender: TObject);
begin
  if Assigned(FServer) then
    FServer.Free;
end;

procedure TFormHttpserver.sbChooseFileDirClick(Sender: TObject);
begin
  if FolderOpenDlg.Execute then
    edFileDir.Text := FolderOpenDlg.FileName;
end;

procedure TFormHttpserver.SpeedButton1Click(Sender: TObject);
begin
  if OpenFileDlg.Execute then
    edCertFile.Text := OpenFileDlg.FileName;
end;

end.


