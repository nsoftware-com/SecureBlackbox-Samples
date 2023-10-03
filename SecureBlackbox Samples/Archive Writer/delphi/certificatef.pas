unit certificatef;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SBxTypes, SBxCertificateManager;

type
  TFormCertificate = class(TForm)
    edtCertFile: TEdit;
    lblCertFile: TLabel;
    btnChooseFile: TButton;
    lblCertificatePassword: TLabel;
    edtCertPassword: TEdit;
    btnLoad: TButton;
    btnCancel: TButton;
    odOpenCertificate: TOpenDialog;
    procedure btnLoadClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnChooseFileClick(Sender: TObject);
    procedure edtCertPasswordKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FCertificateManager: TsbxCertificateManager;

    function GetCertificate: TsbxCertificate;
  public
    { Public declarations }
    property Certificate: TsbxCertificate read GetCertificate;
  end;

var
  FormCertificate: TFormCertificate;

implementation

{$R *.dfm}

procedure TFormCertificate.btnLoadClick(Sender: TObject);
begin
  try
    FCertificateManager.ImportFromFile(edtCertFile.Text, edtCertPassword.Text);

    if Certificate.PrivateKeyExists then
      ModalResult := IDOK
    else
      MessageDlg('There is no private key for this certificate.', mtError, [mbOk], 0);
  except
    on E: Exception do
      MessageDlg('Failed to load certificate (' + E.Message + ')', mtError, [mbOk], 0);
  end;
end;

procedure TFormCertificate.btnCancelClick(Sender: TObject);
begin
  ModalResult := IDCANCEL;
end;

procedure TFormCertificate.btnChooseFileClick(Sender: TObject);
begin
  if odOpenCertificate.Execute then
    edtCertFile.Text := odOpenCertificate.FileName;
end;

procedure TFormCertificate.edtCertPasswordKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then
    btnLoadClick(btnLoad);
end;

procedure TFormCertificate.FormCreate(Sender: TObject);
begin
  FCertificateManager := TsbxCertificateManager.Create(nil);
end;

procedure TFormCertificate.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCertificateManager);
end;

function TFormCertificate.GetCertificate: TsbxCertificate;
begin
  Result := FCertificateManager.Certificate;
end;

end.
