unit addnewuserf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormAddnewuser = class(TForm)
    Label1: TLabel;
    edLogin: TEdit;
    Label2: TLabel;
    edPassword: TEdit;
    bbOK: TButton;
    bbCancel: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAddnewuser: TFormAddnewuser;

implementation

{$R *.dfm}

end.
