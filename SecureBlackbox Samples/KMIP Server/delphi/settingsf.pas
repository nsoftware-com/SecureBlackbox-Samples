unit settingsf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Buttons, ComCtrls,
  SBxTypes, SBxKMIPServer, SBxUserManager, Vcl.ExtCtrls;

type
  TFormsettings = class(TForm)
    btOk: TButton;
    GroupBox1: TGroupBox;
    gbUsers: TGroupBox;
    btnAdd: TBitBtn;
    btnRemove: TBitBtn;
    lbUsers: TListBox;
    Label2: TLabel;
    Label1: TLabel;
    sbBrowseStorage: TSpeedButton;
    edListenPort: TEdit;
    cbUseSSL: TCheckBox;
    edStorageFile: TEdit;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    edCertFile: TEdit;
    Label4: TLabel;
    edCertPassword: TEdit;
    sbBrowseCert: TSpeedButton;
    btCancel: TButton;
    cbUseHTTP: TCheckBox;
    procedure btOkClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbBrowseCertClick(Sender: TObject);
    procedure sbBrowseStorageClick(Sender: TObject);
  private
    { Private declarations }
    FUserManager: TsbxUserManager;
  public
    procedure UpdateSettings(Server: TsbxKMIPServer);

    property UserManager: TsbxUserManager read FUserManager;
  end;

var
  Formsettings: TFormsettings;

implementation

{$R *.dfm}

uses adduserf;

procedure TFormsettings.btnAddClick(Sender: TObject);
var
  Index: integer;
  UserForm: TFormadduser;
begin
  UserForm := TFormadduser.Create(nil);
  try
    if UserForm.ShowModal = mrOk then
    begin
      Index := FUserManager.AddUser(UserForm.edName.Text);
      FUserManager.Users.Item[Index].Password := UserForm.edPassword.Text;

      lbUsers.Items.Add(FUserManager.Users[UserManager.Users.Count - 1].Username);
    end;
  finally
    FreeAndNil(UserForm);
  end;
end;

procedure TFormsettings.btnRemoveClick(Sender: TObject);
begin
  if lbUsers.ItemIndex <> -1 then
  begin
    FUserManager.Users.RemoveAt(lbUsers.ItemIndex);
    lbUsers.Items.Delete(lbUsers.ItemIndex);
  end;
end;

procedure TFormsettings.btOkClick(Sender: TObject);
begin
  if edStorageFile.Text = '' then
  begin
    ShowMessage('Please, enter storage file');
    edStorageFile.SetFocus;
    exit;
  end;

  /// NEVER USE THIS PASSWORD IN REAL PROJECT. IT IS NOT SAFE. EVERYONE KNOWS IT !!!!!!!!!!!!!!
  FUserManager.ExportToFile('Users.dat', 'Asd$%sdf####f.>');

  ModalResult := mrOk;
end;

procedure TFormsettings.FormCreate(Sender: TObject);
begin
  FUserManager := TsbxUserManager.Create(nil);
end;

procedure TFormsettings.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FUserManager);
end;

procedure TFormsettings.sbBrowseCertClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    edCertFile.Text := OpenDialog.FileName;
end;

procedure TFormsettings.sbBrowseStorageClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    edStorageFile.Text := SaveDialog.FileName;
end;

procedure TFormsettings.UpdateSettings(Server: TsbxKMIPServer);
var
  i: integer;
begin
  edStorageFile.Text := Server.StorageFileName;

  edListenPort.Text := IntToStr(Server.Port);

  if Server.TLSSettings.TLSMode = smNoTLS then
    cbUseSSL.Checked := false
  else
    cbUseSSL.Checked := true;

  cbUseHTTP.Checked := Server.UseHTTP;

  // Users
  FUserManager.Users.Clear;
  lbUsers.Clear;

  for i := 0 to Server.Users.Count - 1 do
  begin
    FUserManager.Users.Add(Server.Users.Item[i]);
    lbUsers.Items.Add(Server.Users.Item[i].UserName);
  end;
end;

end.
