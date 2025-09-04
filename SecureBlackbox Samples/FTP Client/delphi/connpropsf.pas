unit connpropsf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TFormConnprops = class(TForm)
    gbConnProps: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    lHost: TLabel;
    editHost: TEdit;
    lUsername: TLabel;
    editUsername: TEdit;
    lPassword: TLabel;
    editPassword: TEdit;
    OpenDialog: TOpenDialog;
    Label1: TLabel;
    rbNoTLS: TRadioButton;
    rbExplicitTLS: TRadioButton;
    rbImplicitTLS: TRadioButton;
    cbPassive: TCheckBox;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
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

procedure TFormConnprops.FormShow(Sender: TObject);
begin
  editPassword.Text := '';
end;

end.
