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
unit webdavserverf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Buttons,
  SBxTypes, SBxWebDAVServer, sbxCertificateManager;

type
  TFormWebdavserver = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    sbChooseFileDir: TSpeedButton;
    Label3: TLabel;
    sbChooseCert: TSpeedButton;
    Label4: TLabel;
    edPort: TEdit;
    cbUseTLS: TCheckBox;
    edFileDir: TEdit;
    edCertFile: TEdit;
    edCertPassword: TEdit;
    bbStart: TButton;
    bbStop: TButton;
    mmLog: TMemo;
    FolderOpenDlg: TFileOpenDialog;
    OpenFileDlg: TOpenDialog;
    Label5: TLabel;
    lvUsers: TListView;
    sbUserAdd: TSpeedButton;
    sbUserDelete: TSpeedButton;
    Label6: TLabel;
    edMetaDir: TEdit;
    sbChooseMetaDir: TSpeedButton;
    procedure sbUserAddClick(Sender: TObject);
    procedure sbUserDeleteClick(Sender: TObject);
    procedure sbChooseFileDirClick(Sender: TObject);
    procedure sbChooseCertClick(Sender: TObject);
    procedure sbChooseMetaDirClick(Sender: TObject);
    procedure bbStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bbStopClick(Sender: TObject);
  private
    { Private declarations }
    FServer: TsbxWebDAVServer;

    procedure DoAccept(Sender: TObject; const RemoteAddress: String;
      RemotePort: Integer; var Accept: Boolean);
    procedure DoBeforeRequest(Sender: TObject; ConnectionID: Int64;
      const HTTPMethod: String; const URL: String; var Accept: Boolean);
  public
    { Public declarations }
  end;

var
  FormWebdavserver: TFormWebdavserver;

implementation

{$R *.dfm}

uses addnewuserf;

procedure TFormWebdavserver.DoAccept(Sender: TObject; const RemoteAddress: String;
  RemotePort: Integer; var Accept: Boolean);
begin
  mmLog.Lines.Add('New connection from ' + RemoteAddress);
  Accept := true;
end;

procedure TFormWebdavserver.DoBeforeRequest(Sender: TObject; ConnectionID: Int64;
  const HTTPMethod: String; const URL: String; var Accept: Boolean);
begin
  mmLog.Lines.Add('[' + IntToStr(ConnectionID) + '] ' + HTTPMethod + ' ' + URL);
  Accept := true;
end;

procedure TFormWebdavserver.bbStartClick(Sender: TObject);
var
  mgr: TsbxCertificateManager;
  Port, i: integer;
  Cert: TsbxCertificate;
  Account: TsbxUserAccount;
begin
  mgr := TsbxCertificateManager.Create(nil);
  if FServer.Active then
    Exit;

  if Length(edFileDir.Text) = 0 then
  begin
    ShowMessage('Choose files folder!');
    Exit;
  end;
  if Length(edMetaDir.Text) = 0 then
  begin
    ShowMessage('Choose metadata folder!');
    Exit;
  end;
  if edFileDir.Text = edMetaDir.Text then
  begin
    ShowMessage('Files and metadata folders should be different!');
    Exit;
  end;

  FServer.DocumentRoot := edFileDir.Text;
  FServer.MetadataRoot := edMetaDir.Text;

  if TryStrToInt(edPort.Text, Port) then
    FServer.Port := Port;

  if cbUseTLS.Checked then
    FServer.TLSSettings.TLSMode := smImplicitTLS
  else
    FServer.TLSSettings.TLSMode := smNoTLS;

  if cbUseTLS.Checked then
  begin
    mgr.ImportFromFile(edCertFile.Text, edCertPassword.Text);
    Cert := mgr.Certificate;
    try
      FServer.TLSServerChain.Add(Cert);
    finally
      Cert.Free;
    end;
  end;

  FServer.Users.Clear;

  for I := 0 to lvUsers.Items.Count - 1 do
  begin
    Account := TsbxUserAccount.Create;
    try
      Account.Username := lvUsers.Items[i].Caption;
      Account.Password := lvUsers.Items[i].SubItems[0];

      FServer.Users.Add(Account);
    finally
      FreeAndNil(Account);
    end;
  end;

  if FServer.Users.Count > 0 then
    FServer.AuthTypes := 1 //haBasic
  else
    FServer.AuthTypes := 0;

  FServer.OnAccept := DoAccept;
  FServer.OnBeforeRequest := DoBeforeRequest;

  FServer.Start;

  bbStart.Enabled := false;
  bbStop.Enabled := true;
end;

procedure TFormWebdavserver.bbStopClick(Sender: TObject);
begin
  if FServer.Active then
  begin
    FServer.Stop;

    bbStart.Enabled := true;
    bbStop.Enabled := false;
  end;
end;

procedure TFormWebdavserver.FormCreate(Sender: TObject);
begin
  FServer := TsbxWebDAVServer.Create(nil);
end;

procedure TFormWebdavserver.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FServer);
end;

procedure TFormWebdavserver.sbChooseCertClick(Sender: TObject);
begin
  OpenFileDlg.Title := 'Select certificate file';
  OpenFileDlg.Filter := 'Certificates (*.pem, *.cer, *.crt, *.der, *.pfx, *.p12, *.pkcs12)|*.pem;*.cer;*.crt;*.der;*.pfx;*.p12;*.pkcs12|PEM-encoded certificates (*.pem)|*.pem|' +
    'DER-encoded certificates (*.cer, *.crt, *.der)|*.cer;*.crt;*.der|PKCS#12 encoded certificates (*.pfx, *.p12, *.pkcs12)|*.pfx;*.p12;*.pkcs12|All files (*.*)|*.*';
  if OpenFileDlg.Execute then
    edCertFile.Text := OpenFileDlg.FileName;
end;

procedure TFormWebdavserver.sbChooseFileDirClick(Sender: TObject);
begin
  if FolderOpenDlg.Execute then
    edFileDir.Text := FolderOpenDlg.FileName;
end;

procedure TFormWebdavserver.sbChooseMetaDirClick(Sender: TObject);
begin
  if FolderOpenDlg.Execute then
    edMetaDir.Text := FolderOpenDlg.FileName;
end;

procedure TFormWebdavserver.sbUserAddClick(Sender: TObject);
var
  Li: TListItem;
begin
  if (FormAddnewuser.ShowModal = mrOK) and
    (FormAddnewuser.edLogin.Text <> '') and
    (FormAddnewuser.edPassword.Text <> '') then
  begin
    Li := lvUsers.Items.Add;
    Li.Caption := FormAddnewuser.edLogin.Text;
    Li.SubItems.Add(FormAddnewuser.edPassword.Text);
  end;
end;

procedure TFormWebdavserver.sbUserDeleteClick(Sender: TObject);
begin
  if lvUsers.ItemIndex >= 0 then
    lvUsers.Items.Delete(lvUsers.ItemIndex);
end;

end.
