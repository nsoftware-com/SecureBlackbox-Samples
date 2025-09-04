unit openarchivef;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormOpenarchive = class(TForm)
    lblArchiveName: TLabel;
    edtArchiveFile: TEdit;
    btnChoose: TButton;
    btnCancel: TButton;
    btnOk: TButton;
    cbArchiveType: TComboBox;
    lblArchiveType: TLabel;
    odArchiveFile: TOpenDialog;
    procedure btnChooseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormOpenarchive: TFormOpenarchive;

implementation

{$R *.dfm}

procedure TFormOpenarchive.btnChooseClick(Sender: TObject);
begin
  if odArchiveFile.Execute then
    edtArchiveFile.Text := odArchiveFile.FileName;
end;


end.
