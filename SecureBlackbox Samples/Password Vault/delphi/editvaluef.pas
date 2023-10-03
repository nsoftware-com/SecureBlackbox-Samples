unit editvaluef;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFormEditValue = class(TForm)
    cbEncrypted: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    Label4: TLabel;
    edFieldName: TEdit;
    edValue: TEdit;
    Label1: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    //
  public
    procedure Init(FieldName, Value: string);
  end;

var
  FormEditValue: TFormEditValue;

implementation

{$R *.DFM}

procedure TFormEditValue.FormShow(Sender: TObject);
begin
  if edFieldName.Enabled then
    edFieldName.SetFocus
  else
    edValue.SetFocus;
end;

procedure TFormEditValue.Init(FieldName, Value: string);
begin
  edFieldName.Text := FieldName;
  edValue.Text := Value;
  cbEncrypted.Checked := false;

  if FieldName = '' then
  begin
    Caption := 'Add new value';
    edFieldName.Enabled := true;
  end
  else
  begin
    Caption := 'Modify value';
    edFieldName.Enabled := false;
  end;
end;

procedure TFormEditValue.btnOKClick(Sender: TObject);
begin
  if edFieldName.Text = '' then
    MessageDlg('Please set field name', mtError, [mbOk], 0)
  else
    ModalResult := mrOk;
end;

end.
