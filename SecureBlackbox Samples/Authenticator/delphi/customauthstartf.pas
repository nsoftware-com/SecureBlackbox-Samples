unit customauthstartf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormCustomAuthStart = class(TForm)
    Label2: TLabel;
    eAuthMethodData: TEdit;
    bOk: TButton;
    bCancel: TButton;
    Label1: TLabel;
    eUserId: TEdit;
    Label3: TLabel;
    eAuthMethod: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCustomAuthStart: TFormCustomAuthStart;

implementation

{$R *.dfm}

end.
