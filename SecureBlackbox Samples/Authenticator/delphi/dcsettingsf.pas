unit dcsettingsf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormDCSettings = class(TForm)
    Label1: TLabel;
    eKeyId: TEdit;
    Label2: TLabel;
    eKeySecret: TEdit;
    Label3: TLabel;
    eData: TEdit;
    bOk: TButton;
    bCancel: TButton;
    Label4: TLabel;
    eUserId: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDCSettings: TFormDCSettings;

implementation

{$R *.dfm}

end.
