unit resultsf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  TFormResults = class(TForm)
    btnOK: TButton;
    mResults: TMemo;
  private
  public
  end;

var
  FormResults: TFormResults;

implementation

{$R *.DFM}

end.
