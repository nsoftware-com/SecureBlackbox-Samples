unit settingsf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, System.IOUtils,
  Controls, Forms, Dialogs, StdCtrls, Buttons, ComCtrls,
  SBxSFTPServer, SBxUserManager;

type
  TFormsettings = class(TForm)
    btOk: TButton;
    gbUsers: TGroupBox;
    btnAdd: TBitBtn;
    btnRemove: TBitBtn;
    lbUsers: TListBox;
    procedure btOkClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
  private
    { Private declarations }
    FServer: TsbxSFTPServer;
    procedure UpdateUsers;
  public
    class procedure Execute(Server: TsbxSFTPServer);
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
  UserManager: TsbxUserManager;
begin
  UserForm := TFormadduser.Create(nil);
  try
    if UserForm.ShowModal = mrOk then
    begin
      UserManager := TsbxUserManager.Create(nil);
      try
        Index := UserManager.AddUser(UserForm.edName.Text);
        UserManager.Users[Index].Password := UserForm.edPassword.Text;
        if FileExists(UserForm.edPublicKey.Text) then
          UserManager.Users[Index].SSHKey := TFile.ReadAllBytes(UserForm.edPublicKey.Text);

        FServer.Users.Add(UserManager.Users[Index]);
      finally
        FreeAndNil(UserManager);
      end;

      UpdateUsers;
    end;
  finally
    FreeAndNil(UserForm);
  end;
end;

procedure TFormsettings.btnRemoveClick(Sender: TObject);
begin
  if lbUsers.ItemIndex <> -1 then
  begin
    FServer.Users.RemoveAt(lbUsers.ItemIndex);
    UpdateUsers;
  end;
end;

procedure TFormsettings.btOkClick(Sender: TObject);
var
  i: integer;
  UserManager: TsbxUserManager;
begin
  UserManager := TsbxUserManager.Create(nil);
  try
    for i := 0 to FServer.Users.Count - 1 do
      UserManager.Users.Add(FServer.Users.Item[i]);
    /// NEVER USE THIS PASSWORD IN REAL PROJECT. IT IS NOT SAFE. EVERYONE KNOWS IT !!!!!!!!!!!!!!
    UserManager.ExportToFile('Users.dat', 'Asd$%sdf####f.>');
  finally
    FreeAndNil(UserManager);
  end;

  ModalResult := mrOk;
end;

class procedure TFormsettings.Execute(Server: TsbxSFTPServer);
var
  Form: TFormsettings;
begin
  Form := TFormsettings.Create(nil);
  try
    Form.FServer := Server;
    Form.UpdateUsers;
    Form.ShowModal;
  finally
    FreeAndNil(Form);
  end;
end;

procedure TFormsettings.UpdateUsers;
var
  i: Integer;
begin
  lbUsers.Clear;

  for i := 0 to FServer.Users.Count - 1 do
    lbUsers.Items.Add(FServer.Users.Item[i].UserName);
end;

end.
