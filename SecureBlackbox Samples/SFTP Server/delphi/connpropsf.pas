unit connpropsf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormConnprops = class(TForm)
    gbConnProps: TGroupBox;
    lPort: TLabel;
    editPort: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  FormConnprops: TFormConnprops;

implementation

{$R *.DFM}

procedure TFormConnprops.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormConnprops.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
