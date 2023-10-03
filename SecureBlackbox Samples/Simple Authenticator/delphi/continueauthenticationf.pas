unit continueauthenticationf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IOUtils, Vcl.StdCtrls;

type
  TFormContinueAuthentication = class(TForm)
    Label2: TLabel;
    eAuthMethod: TEdit;
    bContinue: TButton;
    Label3: TLabel;
    eAuthToken: TEdit;
    bClose: TButton;
    procedure bContinueClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormContinueAuthentication: TFormContinueAuthentication;

implementation

{$R *.dfm}

procedure TFormContinueAuthentication.bContinueClick(Sender: TObject);
begin
  if eAuthToken.Text = '' then
    MessageDlg('Please enter auth token', mtError, [mbOk], 0)
  else
    ModalResult := mrOk;
end;

procedure TFormContinueAuthentication.FormShow(Sender: TObject);
begin
  eAuthToken.SetFocus;
end;

end.
