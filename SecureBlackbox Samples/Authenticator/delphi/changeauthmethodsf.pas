unit changeauthmethodsf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormChangeAuthMethods = class(TForm)
    Label2: TLabel;
    eAuthMethods: TEdit;
    bBaseMethods: TButton;
    bOk: TButton;
    bCancel: TButton;
    Label1: TLabel;
    eUserId: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormChangeAuthMethods: TFormChangeAuthMethods;

implementation

{$R *.dfm}

end.
