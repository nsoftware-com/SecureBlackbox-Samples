unit setpublickeyf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormSetpublickey = class(TForm)
    gbSetPublicKey: TGroupBox;
    memoPublicKey: TMemo;
    lPublicKey: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSetpublickey: TFormSetpublickey;

implementation

{$R *.DFM}

procedure TFormSetpublickey.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormSetpublickey.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
