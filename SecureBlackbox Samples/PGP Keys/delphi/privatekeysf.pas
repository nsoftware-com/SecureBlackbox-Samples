unit privatekeysf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SBPGPKeys;

type
  TFormPrivateKeys = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    lstKeys: TListBox;
    lblPrompt: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormPrivatekeys: TFormPrivateKeys;

implementation

{$R *.dfm}

end.
