unit inputf;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls;

type
  TFormInput = class(TForm)
    lblPrompt: TLabel;
    edtText: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
  private
    function GetText(): string;
    procedure Init(const Prompt: string; Password: Boolean; const Text: string);
  public
    class function Execute(const Prompt: string; Password: Boolean; var Text: string): Boolean;
    property Text: string read GetText;
  end;

var
  FormInput: TFormInput;

implementation

{$R *.dfm}

{ TFormInput }

class function TFormInput.Execute(const Prompt: string; Password: Boolean; var Text: string): Boolean;
var
  Dialog: TFormInput;
begin
  Application.CreateForm(TFormInput, Dialog);
  Dialog.Init(Prompt, Password, Text);
  Result := (Dialog.ShowModal() = mrOk);
  if Result then
    Text := Dialog.Text;
end;

function TFormInput.GetText(): string;
begin
  Result := edtText.Text;
end;

procedure TFormInput.Init(const Prompt: string; Password: Boolean; const Text: string);
begin
  Self.Caption := Application.MainForm.Caption;
  lblPrompt.Caption := Prompt;
  if Password then
    edtText.PasswordChar := '*';
  edtText.Text := Text;
end;

end.
