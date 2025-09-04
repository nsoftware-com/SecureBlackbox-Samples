unit addcertf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormAddCert = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    edCertFile: TEdit;
    bOpenKeyFile: TButton;
    edGroup: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    dlgOpenFile: TOpenDialog;
    edCertPassword: TEdit;
    Label5: TLabel;
    procedure bOpenKeyFileClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAddCert: TFormAddCert;

implementation

{$R *.DFM}

procedure TFormAddCert.bOpenKeyFileClick(Sender: TObject);
begin
  if dlgOpenFile.Execute() then
    edCertFile.Text := dlgOpenFile.FileName;
end;

end.
