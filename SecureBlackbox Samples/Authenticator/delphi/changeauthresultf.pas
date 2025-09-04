unit changeauthresultf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormChangeAuthResult = class(TForm)
    Label1: TLabel;
    eUserId: TEdit;
    Label3: TLabel;
    eAuthMethod: TEdit;
    bOk: TButton;
    bCancel: TButton;
    rbSucceeded: TRadioButton;
    rbFailed: TRadioButton;
    rbFurtherAuthNeeded: TRadioButton;
    Label2: TLabel;
    eAuthMethods: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormChangeAuthResult: TFormChangeAuthResult;

implementation

{$R *.dfm}

end.
