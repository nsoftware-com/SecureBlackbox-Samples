(*
 * SecureBlackbox 2022 Delphi Edition - Sample Project
 *
 * This sample project demonstrates the usage of SecureBlackbox in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/secureblackbox
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 *)
unit pgpkeysf;

interface

uses
  SysUtils, Variants, Classes,
  Controls, Forms, Dialogs, ComCtrls, ToolWin, StdCtrls, ExtCtrls, Menus,
  Graphics, ImgList, Actions, ActnList,
  SBxTypes, SBxCore, SBxPGPKeyring, SBxPGPKeyManager;

type
  TFormPgpkeys = class(TForm)
    aclMain: TActionList;
    actNewKeyring: TAction;
    actOpenKeyring: TAction;
    actSaveKeyring: TAction;
    actExit: TAction;
    actGenerateKey: TAction;
    actAddKey: TAction;
    actRemoveKey: TAction;
    actExportKey: TAction;
    imgToolbar: TImageList;
    imgTreeView: TImageList;
    mnuKeys: TPopupMenu;
    NewKey2: TMenuItem;
    DeleteKey2: TMenuItem;
    OpenDlg: TOpenDialog;
    pInfo: TPanel;
    pKeyInfo: TPanel;
    lbExpires: TLabel;
    lbKeyAlgorithm: TLabel;
    lbKeyFP: TLabel;
    lbKeyID: TLabel;
    lbTimeStamp: TLabel;
    lbTrust: TLabel;
    SaveDlg: TSaveDialog;
    sbrMain: TStatusBar;
    tbMain: TToolBar;
    tbNewKeyring: TToolButton;
    tbDelim1: TToolButton;
    tbLoadKeyring: TToolButton;
    tbSaveKeyring: TToolButton;
    tbDelim2: TToolButton;
    tbGenerate: TToolButton;
    tbDelim3: TToolButton;
    tbAddKey: TToolButton;
    tbRemoveKey: TToolButton;
    tbExportKey: TToolButton;
    tbDelim4: TToolButton;
    tvKeyring: TTreeView;
    Panel1: TPanel;
    Label10: TLabel;
    procedure actExitExecute(Sender: TObject);
    procedure actRemoveKeyExecute(Sender: TObject);
    procedure actRemoveKeyUpdate(Sender: TObject);
    procedure tvKeyringChange(Sender: TObject; Node: TTreeNode);
    procedure actGenerateKeyExecute(Sender: TObject);
    procedure actAddKeyExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actExportKeyExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actNewKeyringExecute(Sender: TObject);
    procedure actOpenKeyringExecute(Sender: TObject);
    procedure actSaveKeyringExecute(Sender: TObject);
  private
    { Private declarations }
    pgpKeyring: TsbxPGPKeyring;
    PublicKeyringFile: string;
    SecretKeyringFile: string;

    procedure SetStatus(s: string);

    procedure DrawPublicKeyProps(key: TsbxPGPKey);

    procedure HideAllInfoPanels();
    procedure EnableView(p: TPanel);
  public
    { Public declarations }
  end;

var
  FormPgpkeys: TFormPgpkeys;

implementation

{$R *.dfm}

uses keyringf, wizardf;

function GetDefaultUserID(Key: TsbxPGPKey): string;
begin
  if Length(Key.Username) > 0 then
    Result := Key.Username
  else
    Result := 'No name';
end;

procedure RedrawKeyring(tv: TTreeView; Keyring: TsbxPGPKeyring);
var
  i, j: Integer;
  Key, SubKey: TsbxPGPKey;
  KeyNode, SubKeyNode: TTreeNode;
begin
  tv.Items.BeginUpdate();

  try
  tv.Items.Clear();
  for i := 0 to Keyring.PublicKeys.Count - 1 do
  begin
    Key := TsbxPGPKey(keyring.PublicKeys.Item[i]);

    if not Key.IsSubkey then
    begin
      // Creating key node
      KeyNode := tv.Items.Add(nil, GetDefaultUserID(Key));
      KeyNode.Data := Key.Clone;
      if Pos('RSA', Key.PublicKeyAlgorithm) >= 1 then
        KeyNode.ImageIndex := 1
      else
        KeyNode.ImageIndex := 0;

      KeyNode.SelectedIndex := KeyNode.ImageIndex;

      // Subkeys
      for j := 0 to Keyring.PublicKeys.Count - 1 do
      begin
        SubKey := TsbxPGPKey(keyring.PublicKeys.Item[j]);

        if SubKey.IsSubkey and (UpperCase(SubKey.PrimaryKeyID) = UpperCase(Key.KeyID)) then
        begin
          SubKeyNode := tv.Items.AddChild(KeyNode, SubKey.PublicKeyAlgorithm + ' subkey');
          SubKeyNode.Data := SubKey.Clone;
        end;
      end;
    end;
  end

  finally
    tv.Items.EndUpdate();
  end;
end;

procedure TFormPgpkeys.actNewKeyringExecute(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to create a new keyring?'#13#10'All unsaved information will be LOST!',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    tvKeyring.Items.Clear();
    pgpKeyring.Clear();
    PublicKeyringFile := '';
    SecretKeyringFile := '';
    HideAllInfoPanels();
    RedrawKeyring(tvKeyring, pgpKeyring);
    SetStatus('New keyring created');
  end;
end;

procedure TFormPgpkeys.actOpenKeyringExecute(Sender: TObject);
begin
  with TFormKeyring.Create(Self) do
    try
      OpenKeyring := True;
      Caption := 'Load keyring';
      if ShowModal = mrOK then
      begin
        try
          if pgpKeyring.Opened then
            pgpKeyring.Close();

          PublicKeyringFile := edtPub.Text;
          SecretKeyringFile := edtSec.Text;
          pgpKeyring.Load(PublicKeyringFile, SecretKeyringFile);

          HideAllInfoPanels();
          RedrawKeyring(tvKeyring, pgpKeyring);
          SetStatus('Keyring loaded');
        except
          on E: Exception do
          begin
            MessageDlg(E.Message, mtError, [mbOK], 0);
            SetStatus('Failed to load keyring');
            Exit;
          end;
        end;
      end;
    finally
      Free;
    end;
end;

procedure TFormPgpkeys.actSaveKeyringExecute(Sender: TObject);
begin
  with TFormKeyring.Create(Self) do
    try
      OpenKeyring := False;
      Caption := 'Save keyring';
      edtPub.Text := PublicKeyringFile;
      edtSec.Text := SecretKeyringFile;
      if (ShowModal = mrOK) then
        try
          PublicKeyringFile := edtPub.Text;
          SecretKeyringFile := edtSec.Text;
          pgpKeyring.Save(PublicKeyringFile, SecretKeyringFile);

          SetStatus('Keyring saved');
        except
          on E: Exception do
     	    begin
            MessageDlg(E.Message, mtError, [mbOK], 0);
            SetStatus('Failed to save keyring');
	        end;
        end;
    finally
      Free;
    end;
end;

procedure TFormPgpkeys.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormPgpkeys.actGenerateKeyExecute(Sender: TObject);
begin
  with TFormWizard.Create(Self) do
    try
      if ShowModal = mrOK then
      begin
        pgpKeyring.PinnedKey := FKeyManager.Key;
        pgpKeyring.AddPinned();

        RedrawKeyring(tvKeyring, pgpKeyring);
        SetStatus('New key was added to keyring');
      end;
    finally
      Free;
    end;
end;

procedure TFormPgpkeys.actAddKeyExecute(Sender: TObject);
begin
  if OpenDlg.Execute then
  begin
    try
      pgpKeyring.AddFromFile(OpenDlg.Filename);

      RedrawKeyring(tvKeyring, pgpKeyring);
      SetStatus('Key(s) successfully imported');
    except
      on E: Exception do
      begin
        MessageDlg(E.Message, mtError, [mbOK], 0);
        SetStatus('Failed to import key');
      end;
    end;
  end;
end;

procedure TFormPgpkeys.actRemoveKeyUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (tvKeyring.Selected <> nil) and
    (tvKeyring.Selected.Level = 0);
end;

procedure TFormPgpkeys.actRemoveKeyExecute(Sender: TObject);
var
  Key: TsbxPGPKey;
begin
  if tvKeyring.Selected <> nil then
  begin
    Key := TsbxPGPKey(tvKeyring.Selected.Data);
    if (MessageDlg('Are you sure you want to remove the key (' + GetDefaultUserID(key) + ')?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      if key.IsSecret then
      begin
        if (MessageDlg('The key you want to remove is SECRET! Are you still sure?',
                  mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
          Exit;
      end;

      try
        pgpKeyring.RemoveByID(Key.KeyID);

        RedrawKeyring(tvKeyring, pgpKeyring);
        SetStatus('Key was successfully removed');
      except
        on E: Exception do
        begin
          MessageDlg(E.Message, mtError, [mbOK], 0);
          SetStatus('Failed to remove key');
        end;
      end;
    end;
  end;
end;

procedure TFormPgpkeys.actExportKeyExecute(Sender: TObject);
var
  KeyManager: TsbxPGPKeyManager;
begin
  if (tvKeyring.Selected <> nil) and (not TsbxPGPKey(tvKeyring.Selected.Data).IsSubkey) then
  begin
    if SaveDlg.Execute then
    begin
      KeyManager := TsbxPGPKeyManager.Create(nil);
      try
        KeyManager.Key := TsbxPGPKey(tvKeyring.Selected.Data);
        KeyManager.ExportToFile(SaveDlg.FileName);

        SetStatus('Key saved');
      finally
        FreeAndNil(KeyManager);
      end;
    end;
  end;
end;

procedure TFormPgpkeys.SetStatus(s: string);
begin
  sbrMain.SimpleText := s;
end;

procedure TFormPgpkeys.HideAllInfoPanels();
begin
  pKeyInfo.Visible := False;
end;

procedure TFormPgpkeys.EnableView(p: TPanel);
begin
  p.Align := alClient;
  p.Visible := True;
end;

procedure TFormPgpkeys.DrawPublicKeyProps(Key: TsbxPGPKey);
begin
  HideAllInfoPanels();
  lbKeyAlgorithm.Caption := 'Algorithm: ' + Key.PublicKeyAlgorithm + ' (' + IntToStr(Key.BitsInKey) + ' bits)';
  lbKeyID.Caption := 'KeyID: ' + Key.KeyID;
  lbKeyFP.Caption := 'KeyFP: ' + Key.KeyFP;
  lbTimestamp.Caption := 'Created: ' + Key.Timestamp;
  if Key.Expires = 0 then
    lbExpires.Caption := 'Expires: NEVER'
  else
    lbExpires.Caption := 'Expires: ' + IntToStr(Key.Expires);

  EnableView(pKeyInfo);
end;

procedure TFormPgpkeys.tvKeyringChange(Sender: TObject; Node: TTreeNode);
begin
  DrawPublicKeyProps(TsbxPGPKey(Node.Data))
end;

procedure TFormPgpkeys.FormCreate(Sender: TObject);
begin
  pgpKeyring := TsbxPGPKeyring.Create(nil);
  pgpKeyring.CreateNew;

  HideAllInfoPanels();
end;

procedure TFormPgpkeys.FormDestroy(Sender: TObject);
begin
  FreeAndNil(pgpKeyring);
end;

end.





