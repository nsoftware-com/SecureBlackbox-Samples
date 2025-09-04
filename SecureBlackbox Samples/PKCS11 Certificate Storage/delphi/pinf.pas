unit pinf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormPin = class(TForm)
    lPrompt: TLabel;
    editPIN: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormPin: TFormPin;

implementation

{$R *.DFM}

procedure TFormPin.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormPin.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormPin.FormShow(Sender: TObject);
begin
  editPIN.Text := '';
  editPIN.SetFocus;
end;

end.
