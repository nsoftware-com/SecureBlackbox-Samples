unit progressf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TFormProgress = class(TForm)
    lblCurrentFile: TLabel;
    lblTotal: TLabel;
    lblCurrentFileName: TLabel;
    pbCurrentFileProgress: TProgressBar;
    pbTotalProgress: TProgressBar;
    btnCancel: TButton;
    lvLog: TListView;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FCancelOperation: boolean;
  public
    procedure AddToLog(const St : string);

    property CancelOperation : boolean read FCancelOperation write FCancelOperation;
    { Public declarations }
  end;

var
  FormProgress: TFormProgress;

implementation

{$R *.dfm}

procedure TFormProgress.btnCancelClick(Sender: TObject);
begin
  FCancelOperation := true;
end;

procedure TFormProgress.btnOkClick(Sender: TObject);
begin
  Hide;
end;

procedure TFormProgress.AddToLog(const St : string);
var
  Item : TListItem;
begin
  Item := lvLog.Items.Add;
  Item.Caption := TimeToStr(Now);
  Item.SubItems.Add(St);
  Item.MakeVisible(false);
end;

procedure TFormProgress.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caNone;
end;

end.
