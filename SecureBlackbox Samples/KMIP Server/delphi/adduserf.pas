unit adduserf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Buttons;

type
  TFormadduser = class(TForm)
    edName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edPassword: TEdit;
    bCancel: TButton;
    bOk: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Formadduser: TFormadduser;

implementation

{$R *.dfm}

end.
