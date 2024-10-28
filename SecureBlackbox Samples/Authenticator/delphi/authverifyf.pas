unit authverifyf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormAuthVerify = class(TForm)
    Label2: TLabel;
    eAuthMethodData: TEdit;
    bValid: TButton;
    bNotValid: TButton;
    Label1: TLabel;
    eUserId: TEdit;
    Label3: TLabel;
    eAuthMethod: TEdit;
    Label4: TLabel;
    eAuthToken: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAuthVerify: TFormAuthVerify;

implementation

{$R *.dfm}

end.
