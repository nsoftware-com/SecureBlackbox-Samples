unit newentryf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormNewEntry = class(TForm)
    Label4: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    edEntryName: TEdit;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormNewEntry: TFormNewEntry;

implementation

{$R *.dfm}

procedure TFormNewEntry.btnOKClick(Sender: TObject);
begin
  if edEntryName.Text = '' then
    MessageDlg('Please set entry name', mtError, [mbOk], 0)
  else
    ModalResult := mrOk;
end;

procedure TFormNewEntry.FormShow(Sender: TObject);
begin
  edEntryName.Text := '';
  edEntryName.SetFocus;
end;

end.
