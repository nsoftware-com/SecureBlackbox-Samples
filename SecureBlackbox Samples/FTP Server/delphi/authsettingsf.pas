unit authsettingsf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ImgList,
  SBxTypes, SBxCore, SBxUserManager, SBxFTPServer;

type
  TFormAuthsettings = class(TForm)
    gbUsers: TGroupBox;
    lUsers: TLabel;
    lvUsers: TListView;
    btnAdd: TBitBtn;
    btnRemove: TBitBtn;
    btnEdit: TBitBtn;
    btnOK: TButton;
    imgUsers: TImageList;
    procedure btnOKClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure lvUsersSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FServer: TsbxFTPServer;
    procedure OutputUserInfo(Item: TListItem; User : TsbxUserAccount);
  public
    procedure Initialize(Server: TsbxFTPServer);
  end;

var
  FormAuthsettings: TFormAuthsettings;

implementation

{$R *.DFM}

uses
  UserSettingsF;

procedure TFormAuthsettings.Initialize(Server: TsbxFTPServer);
var
  i: integer;
begin
  FServer := Server;
  lvUsers.Items.Clear;

  for i := 0 to Server.Users.Count - 1 do
    OutputUserInfo(nil, TsbxUserAccount(Server.Users.Item[i]));
end;

procedure TFormAuthsettings.OutputUserInfo(Item: TListItem; User : TsbxUserAccount);
begin
  if Item = nil then
  begin
    Item := lvUsers.Items.Add;
    Item.ImageIndex := 0;
  end;

  Item.Caption := User.UserName;
  Item.SubItems.Clear;
  Item.SubItems.Add(User.BasePath);
  if User.OutgoingSpeedLimit > 0 then
    Item.SubItems.Add(IntToStr(User.OutgoingSpeedLimit) + 'kb/sec')
  else
    Item.SubItems.Add('unlimited');
end;

procedure TFormAuthsettings.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormAuthsettings.btnAddClick(Sender: TObject);
var
  UserManager: TSBxUserManager;
  Index, SpeedLimit: integer;
begin
  FormUsersettings.Initialize(nil);
  if FormUsersettings.ShowModal = mrOk then
  begin
    if not TryStrToInt(FormUsersettings.editSpeedLimit.Text, SpeedLimit) then
      SpeedLimit := 0;

    UserManager := TSBxUserManager.Create(nil);
    try
      Index := UserManager.AddUser(FormUsersettings.editUsername.Text);
      UserManager.Users[Index].Password := FormUsersettings.editPassword.Text;
      UserManager.Users[Index].IncomingSpeedLimit := SpeedLimit;
      UserManager.Users[Index].OutgoingSpeedLimit := SpeedLimit;
      UserManager.Users[Index].BasePath := FormUsersettings.editHomeDirectory.Text;

      FServer.Users.Add(UserManager.Users[Index]);

      OutputUserInfo(nil, UserManager.Users[Index]);
    finally
      FreeAndNil(UserManager);
    end;
  end;
end;

procedure TFormAuthsettings.btnRemoveClick(Sender: TObject);
begin
  if (lvUsers.Selected <> nil) then
  begin
    FServer.Users.RemoveAt(lvUsers.Selected.Index);
    lvUsers.Items.Delete(lvUsers.Selected.Index);
  end;
end;

procedure TFormAuthsettings.btnEditClick(Sender: TObject);
var
  User: TsbxUserAccount;
begin
  if (lvUsers.Selected <> nil) then
  begin
    User := TsbxUserAccount(FServer.Users.Item[lvUsers.Selected.Index]);
    FormUsersettings.Initialize(User);
    if FormUsersettings.ShowModal = mrOk then
      OutputUserInfo(lvUsers.Selected, User);
  end;
end;

procedure TFormAuthsettings.lvUsersSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  btnRemove.Enabled := (Item <> nil) and Selected;
  btnEdit.Enabled := btnRemove.Enabled; 
end;

end.
