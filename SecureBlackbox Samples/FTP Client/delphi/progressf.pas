unit progressf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TFormProgress = class(TForm)
    gbProgress: TGroupBox;
    btnCancel: TButton;
    lSourceFile: TLabel;
    lDestFile: TLabel;
    lSourceFilename: TLabel;
    lDestFileName: TLabel;
    lProcessed: TLabel;
    pbProgress: TProgressBar;
    lProgress: TLabel;
    procedure btnCancelClick(Sender: TObject);
  private
    FCanceled : boolean;
  public
    property Canceled: boolean read FCanceled write FCanceled;
  end;

var
  FormProgress: TFormProgress;

implementation

{$R *.DFM}

procedure TFormProgress.btnCancelClick(Sender: TObject);
begin
  FCanceled := true;
end;

end.
