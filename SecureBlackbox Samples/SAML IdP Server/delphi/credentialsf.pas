unit credentialsf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls;

type
  TFormCredentials = class(TForm)
    Label1: TLabel;
    edLogin: TEdit;
    Label2: TLabel;
    edPassword: TEdit;
    bbCancel: TButton;
    bbOK: TButton;
    Label3: TLabel;
    edEmail: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCredentials: TFormCredentials;

implementation

{$R *.dfm}

end.
