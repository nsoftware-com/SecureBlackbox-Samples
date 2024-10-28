unit continueauthenticationf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IOUtils, Vcl.StdCtrls;

type
  TFormContinueAuthentication = class(TForm)
    Label1: TLabel;
    eAuthState: TEdit;
    Label2: TLabel;
    eAuthMethod: TEdit;
    bContinue: TButton;
    cbAuthMethodsResult: TCheckBox;
    bLoadState: TButton;
    bSaveState: TButton;
    Label3: TLabel;
    eAuthToken: TEdit;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    procedure bLoadStateClick(Sender: TObject);
    procedure bSaveStateClick(Sender: TObject);
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
  if eAuthState.Text = '' then
    MessageDlg('Please enter auth state', mtError, [mbOk], 0)
  else
  if eAuthToken.Text = '' then
    MessageDlg('Please enter auth token', mtError, [mbOk], 0)
  else
    ModalResult := mrOk;
end;

procedure TFormContinueAuthentication.bLoadStateClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    eAuthState.Text := TFile.ReadAllText(dlgOpen.FileName);
end;

procedure TFormContinueAuthentication.bSaveStateClick(Sender: TObject);
begin
  if dlgSave.Execute then
    TFile.WriteAllText(dlgSave.FileName, eAuthState.Text);
end;

procedure TFormContinueAuthentication.FormShow(Sender: TObject);
begin
  if eAuthState.Text = '' then
    eAuthState.SetFocus
  else
    eAuthToken.SetFocus;
end;

end.
