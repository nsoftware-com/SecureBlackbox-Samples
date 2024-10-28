unit addkeyf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormAddKey = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    edKeyFile: TEdit;
    bOpenKeyFile: TButton;
    edGroup: TEdit;
    Label1: TLabel;
    Label7: TLabel;
    cbCurve: TComboBox;
    Label2: TLabel;
    dlgOpenFile: TOpenDialog;
    procedure bOpenKeyFileClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAddKey: TFormAddKey;

implementation

{$R *.DFM}

procedure TFormAddKey.bOpenKeyFileClick(Sender: TObject);
begin
  if dlgOpenFile.Execute() then
    edKeyFile.Text := dlgOpenFile.FileName;
end;

end.
