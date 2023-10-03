unit certloadf;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, sbxCertificateManager;

type
  TFormCertLoad = class(TForm)
    lblPrompt: TLabel;
    lblIssuer: TLabel;
    edtIssuer: TEdit;
    lblSerialNumber: TLabel;
    lblKeyID: TLabel;
    txtSerialNumber: TStaticText;
    txtKeyID: TStaticText;
    grpCertificate: TGroupBox;
    lblFilename: TLabel;
    edtFilename: TEdit;
    btnSelect: TButton;
    dlgLoadCert: TOpenDialog;
    lblCertIssuer: TLabel;
    edtCertIssuer: TEdit;
    lblCertSerialNumber: TLabel;
    txtCertSerialNumber: TStaticText;
    lblCertKeyID: TLabel;
    txtCertKeyID: TStaticText;
    lblCertPrivateKey: TLabel;
    txtCertPrivateKey: TStaticText;
    btnLoad: TButton;
    btnOk: TButton;
    btnCancel: TButton;
    lblPassword: TLabel;
    edtPassword: TEdit;
    procedure btnSelectClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
  private
    FCertManager: TsbxCertificateManager;
    procedure SetIssuerRDN(const Value: string);
    procedure SetKeyID(const Value: TBytes);
    procedure SetSerialNumber(const Value: TBytes);
  public
    class function Execute(Manager: TsbxCertificateManager; const IssuerRDN: string;
      const SerialNumber, KeyID: TBytes): Boolean;
    property CertManager: TsbxCertificateManager read FCertManager write FCertManager;
    property IssuerRDN: string write SetIssuerRDN;
    property SerialNumber: TBytes write SetSerialNumber;
    property KeyID: TBytes write SetKeyID;
  end;

  var
    FormCertLoad : TFormCertLoad;

implementation

{$R *.dfm}

function BinToHex(const Value: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(Value) - 1 do
  begin
    if Length(Result) <> 0 then
      Result := Result + ' ';
    Result := Result + IntToHex(Value[I], 2);
  end;
end;

procedure TFormCertLoad.btnLoadClick(Sender: TObject);
begin
  try
    FCertManager.ImportFromFile(edtFilename.Text, edtPassword.Text);
  except
    on E: Exception do
    begin
      MessageDlg('Failed load a certificate.'#13#10 + E.Message, mtError, [mbOk], 0);
      Exit;
    end;
  end;

  edtCertIssuer.Text := FCertManager.Certificate.IssuerRDN;
  txtCertSerialNumber.Caption := BinToHex(FCertManager.Certificate.SerialNumber);
  txtCertKeyID.Caption := BinToHex(FCertManager.Certificate.SubjectKeyID);
  if FCertManager.Certificate.PrivateKeyExists then
    txtCertPrivateKey.Caption := 'available'
  else
    txtCertPrivateKey.Caption := 'NOT AVAILABLE';
end;

procedure TFormCertLoad.btnSelectClick(Sender: TObject);
begin
  if dlgLoadCert.Execute(Self.Handle) then
    edtFilename.Text := dlgLoadCert.FileName;
end;

class function TFormCertLoad.Execute(Manager: TsbxCertificateManager; const IssuerRDN: string;
  const SerialNumber, KeyID: TBytes): Boolean;
var
  Dialog: TFormCertLoad;
begin
  Application.CreateForm(TFormCertLoad, Dialog);
  Dialog.CertManager := Manager;
  Dialog.IssuerRDN := IssuerRDN;
  Dialog.SerialNumber := SerialNumber;
  Dialog.KeyID := KeyID;
  Result := (Dialog.ShowModal() = mrOk);
  FreeAndNil(Dialog);
end;

procedure TFormCertLoad.SetIssuerRDN(const Value: string);
begin
  edtIssuer.Text := Value;
end;

procedure TFormCertLoad.SetKeyID(const Value: TBytes);
begin
  txtKeyID.Caption := BinToHex(Value);
end;

procedure TFormCertLoad.SetSerialNumber(const Value: TBytes);
begin
  txtSerialNumber.Caption := BinToHex(Value);
end;

end.