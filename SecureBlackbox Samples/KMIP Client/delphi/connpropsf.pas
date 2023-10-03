unit connpropsf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Vcl.ExtCtrls;

type
  TFormConnprops = class(TForm)
    gbConnProps: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    lHost: TLabel;
    editHost: TEdit;
    lUsername: TLabel;
    editPort: TEdit;
    OpenDialog: TOpenDialog;
    rgEncoder: TRadioGroup;
    editPassword: TEdit;
    Label1: TLabel;
    editUsername: TEdit;
    Label2: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
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
