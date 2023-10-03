unit referencesf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls;

type
  TFormReferences = class(TForm)
    btnClose: TButton;
    lvReferenceResults: TListView;
  private
  public
  end;

var
  FormReferences: TFormReferences;

implementation

{$R *.dfm}

{ TfrmReferences }

end.