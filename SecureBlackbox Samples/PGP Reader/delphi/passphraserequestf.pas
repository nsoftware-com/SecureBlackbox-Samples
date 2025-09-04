unit passphraserequestf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormPassphraserequest = class(TForm)
    lbPrompt: TLabel;
    edPassphrase: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    lbKeyID: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormPassphraserequest: TFormPassphraserequest;

implementation

{$R *.DFM}

end.
