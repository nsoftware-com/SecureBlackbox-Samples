unit promptf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormPrompt = class(TForm)
    lblPrompt: TLabel;
    edtResponse: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
    class function Prompt(const Request : string; ShowResponse : boolean; var Response : string) : boolean;
  end;

var
  FormPrompt: TFormPrompt;

implementation

{$R *.DFM}

class function TFormPrompt.Prompt(const Request : string; ShowResponse : boolean; var Response : string) : boolean;
var Instance : TFormPrompt;
begin
  Instance := TFormPrompt.Create(nil);
  try
    Instance.lblPrompt.Caption := Request;
    if not ShowResponse then
      Instance.edtResponse.PasswordChar := '*';

    result := Instance.ShowModal  = mrOk;
    if Result then
      Response := Instance.edtResponse.Text;
  finally
    FreeAndNil(Instance);
  end;
end;

end.
