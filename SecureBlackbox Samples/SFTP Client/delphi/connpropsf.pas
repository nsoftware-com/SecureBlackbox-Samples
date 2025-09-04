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
    lbPrivateKey: TLabel;
    edPrivateKey: TEdit;
    sbPrivateKey: TSpeedButton;
    OpenDialog: TOpenDialog;
    Label1: TLabel;
    edKeyPassword: TEdit;
    Label2: TLabel;
    edTrustedKeys: TEdit;
    cbTrustedKeys: TSpeedButton;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbPrivateKeyClick(Sender: TObject);
    procedure cbTrustedKeysClick(Sender: TObject);
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

procedure TFormConnprops.cbTrustedKeysClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    edTrustedKeys.Text := OpenDialog.Filename;
end;

procedure TFormConnprops.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormConnprops.FormShow(Sender: TObject);
begin
  editPassword.Text := '';
end;

procedure TFormConnprops.sbPrivateKeyClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    edPrivateKey.Text := OpenDialog.Filename;
end;

end.
