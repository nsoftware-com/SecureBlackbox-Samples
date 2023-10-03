unit serversettingsf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormServersettings = class(TForm)
    gbServerSettings: TGroupBox;
    btnOK: TButton;
    lCertificateFile: TLabel;
    editCertificateFile: TEdit;
    lPassword: TLabel;
    editCertificatePassword: TEdit;
    buttonSelectCertificateFile: TButton;
    cbImplicitSSL: TCheckBox;
    cbRequireTLS: TCheckBox;
    cbRequireTLSForData: TCheckBox;
    cbAllowAnonymous: TCheckBox;
    odOpenCertificate: TOpenDialog;
    editPassiveModeHost: TEdit;
    Label1: TLabel;
    lPortRange: TLabel;
    editPortsFrom: TEdit;
    Label2: TLabel;
    editPortsTo: TEdit;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure buttonSelectCertificateFileClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormServersettings: TFormServersettings;

implementation

{$R *.DFM}

uses ftpserverf;

procedure TFormServersettings.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
  Settings.CertificateFile := editCertificateFile.Text;
  Settings.CertificatePassword := editCertificatePassword.Text;
  Settings.ImplicitSSL := cbImplicitSSL.Checked;
  Settings.RequireTLS := cbRequireTLS.Checked;
  Settings.RequireTLSForData := cbRequireTLSForData.Checked;
  Settings.AllowAnonymous := cbAllowAnonymous.Checked;
  Settings.PassiveModeHost := editPassiveModeHost.Text;
  Settings.PortRangeFrom := StrToIntDef(editPortsFrom.Text, 0);
  Settings.PortRangeTo := StrToIntDef(editPortsTo.Text, 0);
  Settings.Save;
end;


procedure TFormServersettings.FormShow(Sender: TObject);
begin
  editCertificateFile.Text := Settings.CertificateFile;
  editCertificatePassword.Text := Settings.CertificatePassword;
  editPassiveModeHost.Text := Settings.PassiveModeHost;
  cbImplicitSSL.Checked := Settings.ImplicitSSL;
  cbRequireTLS.Checked := Settings.RequireTLS;
  cbRequireTLSForData.Checked := Settings.RequireTLSForData;
  cbAllowAnonymous.Checked := Settings.AllowAnonymous;
  editPortsFrom.Text := IntToStr(Settings.PortRangeFrom);
  editPortsTo.Text := IntToStr(Settings.PortRangeTo);  
end;

procedure TFormServersettings.buttonSelectCertificateFileClick(
  Sender: TObject);
begin
  odOpenCertificate.FileName := editCertificateFile.Text;
  if odOpenCertificate.Execute then
    editCertificateFile.Text := odOpenCertificate.FileName;
end;

end.
