unit keyringloadf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormKeyringload = class(TForm)
    lblPublicKeyring: TLabel;
    editPubKeyring: TEdit;
    lblSecretKeyring: TLabel;
    editSecKeyring: TEdit;
    btnBrowsePub: TButton;
    btnBrowseSec: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    OpenDialog: TOpenDialog;
    procedure btnBrowsePubClick(Sender: TObject);
    procedure btnBrowseSecClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormKeyringload: TFormKeyringload;

implementation

{$R *.DFM}

procedure TFormKeyringload.btnBrowsePubClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    editPubKeyring.Text := OpenDialog.Filename;
end;

procedure TFormKeyringload.btnBrowseSecClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    editSecKeyring.Text := OpenDialog.Filename;
end;

procedure TFormKeyringload.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormKeyringload.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
