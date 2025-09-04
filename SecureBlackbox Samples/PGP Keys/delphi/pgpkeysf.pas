(*
 * SecureBlackbox 2024 Delphi Edition - Sample Project
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
  SBxTypes, SBxCore, SBxPGPKeyring, SBxPGPKeyManager, System.ImageList;

type
  TPGPSignature = class(TObject)
  private
    FSig: TsbxPGPSignature;
    FValidRes: string;
  public
    constructor Create(ASig: TsbxPGPSignature; AValidRes: string);
    destructor Destroy; override;

    property Sig: TsbxPGPSignature read FSig;
    property ValidRes: string read FValidRes;
  end;

  TFormPgpkeys = class(TForm)
    imgToolbar: TImageList;
    imgTreeView: TImageList;
    OpenDlg: TOpenDialog;
    pInfo: TPanel;
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
    mnuKeys: TPopupMenu;
    NewKey2: TMenuItem;
    DeleteKey2: TMenuItem;
    N3: TMenuItem;
    Sign2: TMenuItem;
    Revoke1: TMenuItem;
    tbSign: TToolButton;
    aclMain: TActionList;
    actNewKeyring: TAction;
    actOpenKeyring: TAction;
    actSaveKeyring: TAction;
    actExit: TAction;
    actGenerateKey: TAction;
    actAddKey: TAction;
    actRemoveKey: TAction;
    actExportKey: TAction;
    actSign: TAction;
    actRevoke: TAction;
    actAbout: TAction;
    tbRevoke: TToolButton;
    pKeyInfo: TPanel;
    lbKeyAlgorithm: TLabel;
    lbKeyID: TLabel;
    lbKeyFP: TLabel;
    lbTimeStamp: TLabel;
    lbExpires: TLabel;
    pSigInfo: TPanel;
    lbSigType: TLabel;
    lbSigner: TLabel;
    lbSigCreated: TLabel;
    lbValidity: TLabel;
    pUserInfo: TPanel;
    lbUserName: TLabel;
    procedure actExitExecute(Sender: TObject);
    procedure actRemoveKeyExecute(Sender: TObject);
    procedure tvKeyringChange(Sender: TObject; Node: TTreeNode);
    procedure actGenerateKeyExecute(Sender: TObject);
    procedure actAddKeyExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actExportKeyExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actNewKeyringExecute(Sender: TObject);
    procedure actOpenKeyringExecute(Sender: TObject);
    procedure actSaveKeyringExecute(Sender: TObject);
    procedure actRevokeExecute(Sender: TObject);
    procedure actSignExecute(Sender: TObject);
    procedure actExportKeyUpdate(Sender: TObject);
    procedure actRemoveKeyUpdate(Sender: TObject);
    procedure actSignUpdate(Sender: TObject);
  private
    { Private declarations }
    pgpKeyring: TsbxPGPKeyring;
    PublicKeyringFile: string;
    SecretKeyringFile: string;

    procedure SetStatus(s: string);
    procedure HideAllInfoPanels();
    procedure EnableView(p: TPanel);

    procedure RemoveTreeNode(Node: TTreeNode);
    procedure ClearKeyringTree();
    function GetSubKeyIndex(KeyManager: TsbxPGPKeyManager; Key: TsbxPGPKey): integer;
    function GetUserIndex(KeyManager: TsbxPGPKeyManager; User: TsbxPGPUser): integer;
    function GetSignatureIndex(KeyManager: TsbxPGPKeyManager; Sig: TsbxPGPSignature): integer;
    procedure AddSignature(SigNode: TTreeNode; KeyManager: TsbxPGPKeyManager; sigIndex: integer);

    function GetMainKey(ANode: TTreeNode): TsbxPGPKey;
    function GetDefaultUserID(Key: TsbxPGPKey): string;
    procedure RedrawKeyring();

    function RequestPassphrase(UserID, KeyID: string): string;
    procedure DoKeyPassphraseNeeded(Sender: TObject; const KeyID: String; const UserID: String;
      MainKey: Boolean; var Passphrase: String; var Skip: Boolean);
  public
    { Public declarations }
  end;

var
  FormPgpkeys: TFormPgpkeys;

implementation

{$R *.dfm}

uses keyringf, wizardf, privatekeysf, passphraserequestf;

constructor TPGPSignature.Create(ASig: TsbxPGPSignature; AValidRes: string);
begin
  inherited Create();

  FSig := ASig.Clone();
  FValidRes := AValidRes;
end;

destructor TPGPSignature.Destroy;
begin
  FreeAndNil(FSig);
  inherited;
end;

procedure TFormPgpkeys.FormCreate(Sender: TObject);
begin
  pgpKeyring := TsbxPGPKeyring.Create(nil);

  HideAllInfoPanels();
end;

procedure TFormPgpkeys.FormDestroy(Sender: TObject);
begin
  FreeAndNil(pgpKeyring);
end;

procedure TFormPgpkeys.tvKeyringChange(Sender: TObject; Node: TTreeNode);
begin
  HideAllInfoPanels();

  if TObject(Node.Data) is TsbxPGPKey then
  begin
    lbKeyAlgorithm.Caption := 'Algorithm: ' + TsbxPGPKey(Node.Data).PublicKeyAlgorithm + ' (' + IntToStr(TsbxPGPKey(Node.Data).BitsInKey) + ' bits)';
    lbKeyID.Caption := 'KeyID: ' + TsbxPGPKey(Node.Data).KeyID;
    lbKeyFP.Caption := 'KeyFP: ' + TsbxPGPKey(Node.Data).KeyFP;
    lbTimestamp.Caption := 'Created: ' + TsbxPGPKey(Node.Data).Timestamp;
    lbExpires.Caption := 'ValidTo: ' + TsbxPGPKey(Node.Data).ValidTo;
    EnableView(pKeyInfo);
  end
  else if TObject(Node.Data) is TsbxPGPUser then
  begin
    lbUserName.Caption := 'User name: ' + TsbxPGPUser(Node.Data).UserName;
    EnableView(pUserInfo);
  end
  else if TObject(Node.Data) is TPGPSignature then
  begin
    lbSigner.Caption := 'Signer: ' + TPGPSignature(Node.Data).Sig.SignerKeyID;
    lbSigCreated.Caption := 'Created: ' + TPGPSignature(Node.Data).Sig.CreationTime;
    lbValidity.Caption := 'Validity: ' + TPGPSignature(Node.Data).ValidRes;
    if TPGPSignature(Node.Data).Sig.Revocation then
      lbSigType.Caption := 'Type: User revocation'
    else
      lbSigType.Caption := 'Type: Certification signature';
    EnableView(pSigInfo);
  end;
end;

procedure TFormPgpkeys.actNewKeyringExecute(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to reset keyring?'#13#10'All unsaved information will be LOST!',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    tvKeyring.Items.Clear();
    pgpKeyring.Reset;
    PublicKeyringFile := '';
    SecretKeyringFile := '';
    HideAllInfoPanels();
    RedrawKeyring();
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
          pgpKeyring.Reset;

          PublicKeyringFile := edtPub.Text;
          SecretKeyringFile := edtSec.Text;
          pgpKeyring.ImportFromFile(PublicKeyringFile);
          pgpKeyring.ImportFromFile(SecretKeyringFile);

          HideAllInfoPanels();
          RedrawKeyring();
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
          if Length(edtPub.Text) > 0 then
          begin
            PublicKeyringFile := edtPub.Text;
            pgpKeyring.ExportToFile(PublicKeyringFile, false);
          end;
          if Length(edtSec.Text) > 0 then
          begin
            SecretKeyringFile := edtSec.Text;
            pgpKeyring.ExportToFile(SecretKeyringFile, true);
          end;

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

procedure TFormPgpkeys.actGenerateKeyExecute(Sender: TObject);
begin
  with TFormWizard.Create(Self) do
    try
      if ShowModal = mrOK then
      begin
        pgpKeyring.PinnedKey := FKeyManager.Key;
        pgpKeyring.ImportPinned();

        RedrawKeyring();
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
      pgpKeyring.ImportFromFile(OpenDlg.Filename);

      RedrawKeyring();
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
  (Sender as TAction).Enabled := (tvKeyring.Selected <> nil);
end;

procedure TFormPgpkeys.actRemoveKeyExecute(Sender: TObject);
var
  Index: integer;
  Key: TsbxPGPKey;
  User: TsbxPGPUser;
  KeyManager: TsbxPGPKeyManager;
begin
  if tvKeyring.Selected <> nil then
  begin
    if TObject(tvKeyring.Selected.Data) is TsbxPGPKey then
    begin
      Key := TsbxPGPKey(tvKeyring.Selected.Data);
      if (MessageDlg('Are you sure you want to remove the key (' + tvKeyring.Selected.Text + ')?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        if key.IsSecret then
        begin
          if (MessageDlg('The key you want to remove is SECRET! Are you still sure?',
            mtConfirmation, [mbYes, mbNo], 0) <> mrYes)
          then
            Exit;
        end;

        try
          pgpKeyring.RemoveByID(Key.KeyID);

          RemoveTreeNode(tvKeyring.Selected);
          SetStatus('Key was successfully removed');
        except
          on E: Exception do
          begin
            MessageDlg(E.Message, mtError, [mbOK], 0);
            SetStatus('Failed to remove key');
          end;
        end;
      end;
    end
    else if TObject(tvKeyring.Selected.Data) is TsbxPGPUser then
    begin
      User := TsbxPGPUser(tvKeyring.Selected.Data);
      if (MessageDlg('Are you sure you want to remove the user (' + User.Username + ')?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        KeyManager := TsbxPGPKeyManager.Create(nil);
        try
          KeyManager.PinnedKey := GetMainKey(tvKeyring.Selected);
          KeyManager.ImportPinned;

          Index := GetUserIndex(KeyManager, User);
          try
            KeyManager.RemoveUser(Index);
          except
            on E: Exception do
            begin
              MessageDlg(E.Message, mtError, [mbOK], 0);
              SetStatus('Failed to remove user');
              exit;
            end;
          end;

          try
            pgpKeyring.PinnedKey := KeyManager.Key;
            pgpKeyring.UpdatePinned;

            RemoveTreeNode(tvKeyring.Selected);
          except
            on E: Exception do
            begin
              MessageDlg(E.Message, mtError, [mbOK], 0);
              SetStatus('Failed to remove user');
            end;
          end;
        finally
          FreeAndNil(KeyManager);
        end;
      end;
    end
    else if TObject(tvKeyring.Selected.Data) is TPGPSignature then
    begin
      if (MessageDlg('Are you sure you want to remove the signature?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        KeyManager := TsbxPGPKeyManager.Create(nil);
        try
          KeyManager.PinnedKey := GetMainKey(tvKeyring.Selected);
          KeyManager.ImportPinned;

          Index := GetSignatureIndex(KeyManager, TPGPSignature(tvKeyring.Selected.Data).Sig);
          try
            KeyManager.RemoveSignature(Index);
          except
            on E: Exception do
            begin
              MessageDlg(E.Message, mtError, [mbOK], 0);
              SetStatus('Failed to remove signature');
              exit;
            end;
          end;

          try
            pgpKeyring.PinnedKey := KeyManager.Key;
            pgpKeyring.UpdatePinned;

            RemoveTreeNode(tvKeyring.Selected);
          except
            on E: Exception do
            begin
              MessageDlg(E.Message, mtError, [mbOK], 0);
              SetStatus('Failed to remove signature');
            end;
          end;
        finally
          FreeAndNil(KeyManager);
        end;
      end;
    end;
  end;
end;

procedure TFormPgpkeys.actExportKeyUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (tvKeyring.Selected <> nil) and
    (tvKeyring.Selected.Level = 0);
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
        KeyManager.PinnedKey := TsbxPGPKey(tvKeyring.Selected.Data);
        KeyManager.ImportPinned;

        KeyManager.ExportToFile(SaveDlg.FileName, false);

        SetStatus('Key saved');
      finally
        FreeAndNil(KeyManager);
      end;
    end;
  end;
end;

procedure TFormPgpkeys.actSignUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (tvKeyring.Selected <> nil) and
    (tvKeyring.Selected.Level = 1); // only subkeys and users
end;

procedure TFormPgpkeys.actSignExecute(Sender: TObject);
var
  i, Index, signKeyIndex: integer;
  KeyManager: TsbxPGPKeyManager;
begin
  if tvKeyring.Selected <> nil then
  begin
    FormPrivatekeys.lstKeys.Items.Clear();
    for i := 0 to pgpKeyring.Keys.Count - 1 do
      if pgpKeyring.Keys[i].IsSecret and not pgpKeyring.Keys[i].IsSubkey then
        FormPrivatekeys.lstKeys.Items.AddObject(pgpKeyring.Keys[i].Username, TObject(i));

    if FormPrivatekeys.lstKeys.Items.Count = 0 then
    begin
      MessageDlg('There is no secret keys', mtError, [mbOK], 0);
      Exit;
    end;

    if FormPrivatekeys.ShowModal = mrOK then
    begin
      signKeyIndex := 0;
      while (signKeyIndex < FormPrivatekeys.lstKeys.Items.Count) and (not FormPrivatekeys.lstKeys.Selected[signKeyIndex]) do
        Inc(signKeyIndex);

      if signKeyIndex >= FormPrivatekeys.lstKeys.Items.Count then
      begin
        MessageDlg('There is no secret key selected', mtError, [mbOK], 0);
        Exit;
      end;

      KeyManager := TsbxPGPKeyManager.Create(nil);
      try
        KeyManager.OnKeyPassphraseNeeded := DoKeyPassphraseNeeded;
        KeyManager.PinnedKey := GetMainKey(tvKeyring.Selected);
        KeyManager.ImportPinned;

        KeyManager.SigningKey := pgpKeyring.Keys[Integer(FormPrivatekeys.lstKeys.Items.Objects[signKeyIndex])];

        if TObject(tvKeyring.Selected.Data) is TsbxPGPKey then
        begin
          Index := GetSubKeyIndex(KeyManager, TsbxPGPKey(tvKeyring.Selected.Data));
          try
            KeyManager.SignSubkey(Index, '', '', '', 0);
          except
            on E: Exception do
            begin
              MessageDlg(E.Message, mtError, [mbOK], 0);
              SetStatus('Failed to sign subkey');
              exit;
            end;
          end;
        end
        else if TObject(tvKeyring.Selected.Data) is TsbxPGPUser then
        begin
          Index := GetUserIndex(KeyManager, TsbxPGPUser(tvKeyring.Selected.Data));
          try
            KeyManager.SignUser(Index, '', '', true, 0);
          except
            on E: Exception do
            begin
              MessageDlg(E.Message, mtError, [mbOK], 0);
              SetStatus('Failed to sign user');
              exit;
            end;
          end;
        end;

        try
          pgpKeyring.PinnedKey := KeyManager.Key;
          pgpKeyring.UpdatePinned;
        except
          on E: Exception do
          begin
            MessageDlg(E.Message, mtError, [mbOK], 0);
            SetStatus('Failed to add signature to keyring');
          end;
        end;

        RedrawKeyring();
      finally
        FreeAndNil(KeyManager);
      end;
    end;
  end;
end;

procedure TFormPgpkeys.actRevokeExecute(Sender: TObject);
var
  i, Index, signKeyIndex: integer;
  KeyManager: TsbxPGPKeyManager;
begin
  if tvKeyring.Selected <> nil then
  begin
    FormPrivatekeys.lstKeys.Items.Clear();
    for i := 0 to pgpKeyring.Keys.Count - 1 do
      if pgpKeyring.Keys[i].IsSecret and not pgpKeyring.Keys[i].IsSubkey then
        FormPrivatekeys.lstKeys.Items.AddObject(pgpKeyring.Keys[i].Username, TObject(i));

    if FormPrivatekeys.lstKeys.Items.Count = 0 then
    begin
      MessageDlg('There is no secret keys', mtError, [mbOK], 0);
      Exit;
    end;

    if FormPrivatekeys.ShowModal = mrOK then
    begin
      signKeyIndex := 0;
      while (signKeyIndex < FormPrivatekeys.lstKeys.Items.Count) and (not FormPrivatekeys.lstKeys.Selected[signKeyIndex]) do
        Inc(signKeyIndex);

      if signKeyIndex >= FormPrivatekeys.lstKeys.Items.Count then
      begin
        MessageDlg('There is no secret key selected', mtError, [mbOK], 0);
        Exit;
      end;

      KeyManager := TsbxPGPKeyManager.Create(nil);
      try
        KeyManager.OnKeyPassphraseNeeded := DoKeyPassphraseNeeded;
        KeyManager.PinnedKey := GetMainKey(tvKeyring.Selected);
        KeyManager.ImportPinned;

        KeyManager.SigningKey := pgpKeyring.Keys[Integer(FormPrivatekeys.lstKeys.Items.Objects[signKeyIndex])];

        if TObject(tvKeyring.Selected.Data) is TsbxPGPKey then
        begin
          Index := GetSubKeyIndex(KeyManager, TsbxPGPKey(tvKeyring.Selected.Data));
          try
            KeyManager.RevokeSubkey(Index, 0, '');
          except
            on E: Exception do
            begin
              MessageDlg(E.Message, mtError, [mbOK], 0);
              SetStatus('Failed to revoke subkey');
              exit;
            end;
          end;
        end
        else if TObject(tvKeyring.Selected.Data) is TsbxPGPUser then
        begin
          Index := GetUserIndex(KeyManager, TsbxPGPUser(tvKeyring.Selected.Data));
          try
            KeyManager.RevokeUser(Index, 0, '');
          except
            on E: Exception do
            begin
              MessageDlg(E.Message, mtError, [mbOK], 0);
              SetStatus('Failed to revoke user');
              exit;
            end;
          end;
        end;

        try
          pgpKeyring.PinnedKey := KeyManager.Key;
          pgpKeyring.UpdatePinned;
        except
          on E: Exception do
          begin
            MessageDlg(E.Message, mtError, [mbOK], 0);
            SetStatus('Failed to add signature to keyring');
          end;
        end;

        RedrawKeyring();
      finally
        FreeAndNil(KeyManager);
      end;
    end;
  end;
end;

procedure TFormPgpkeys.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormPgpkeys.SetStatus(s: string);
begin
  sbrMain.SimpleText := s;
end;

procedure TFormPgpkeys.HideAllInfoPanels();
begin
  pKeyInfo.Visible := False;
  pUserInfo.Visible := False;
  pSigInfo.Visible := False;
end;

procedure TFormPgpkeys.EnableView(p: TPanel);
begin
  p.Align := alClient;
  p.Visible := True;
end;

procedure TFormPgpkeys.RemoveTreeNode(Node: TTreeNode);
var
  dat: Pointer;
begin
  if Node.Level > 0 then
  begin
    dat := Node.Data;
    FreeAndNil(dat);
  end;

  tvKeyring.Items.Delete(Node);
end;

procedure TFormPgpkeys.ClearKeyringTree();
var
  i: integer;
  dat: Pointer;
begin
  for i := 0 to tvKeyring.Items.Count - 1 do
    if tvKeyring.Items[i].Level > 0 then
    begin
      dat := tvKeyring.Items[i].Data;
      FreeAndNil(dat);
    end;

  tvKeyring.Items.Clear();
end;

function TFormPgpkeys.GetSubKeyIndex(KeyManager: TsbxPGPKeyManager; Key: TsbxPGPKey): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to KeyManager.Subkeys.Count - 1 do
  begin
    if KeyManager.Subkeys[i].KeyID = Key.KeyID then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TFormPgpkeys.GetUserIndex(KeyManager: TsbxPGPKeyManager; User: TsbxPGPUser): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to KeyManager.Users.Count - 1 do
  begin
    if KeyManager.Users[i].Username = User.Username then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TFormPgpkeys.GetSignatureIndex(KeyManager: TsbxPGPKeyManager; Sig: TsbxPGPSignature): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to KeyManager.Signatures.Count - 1 do
  begin
    if KeyManager.Signatures[i].HashMark = Sig.HashMark then
    begin
      Result := i;
      break;
    end;
  end;
end;

procedure TFormPgpkeys.AddSignature(SigNode: TTreeNode; KeyManager: TsbxPGPKeyManager; sigIndex: integer);
var
  i: integer;
  sig: TPGPSignature;
  ValidRes: string;
begin
  ValidRes := 'Unable to verify';
  for i := 0 to pgpKeyring.Keys.Count - 1 do
    if pgpKeyring.Keys[i].KeyID = KeyManager.Signatures[sigIndex].SignerKeyID then
    begin
      KeyManager.SigningKey := pgpKeyring.Keys[i];
      if KeyManager.Verify(sigIndex) then
        ValidRes := 'Valid'
      else
        ValidRes := 'INVALID';

      break;
    end;

  sig := TPGPSignature.Create(KeyManager.Signatures[sigIndex], ValidRes);
  SigNode.Data := sig;
end;

function TFormPgpkeys.GetMainKey(ANode: TTreeNode): TsbxPGPKey;
var
  ParentNode: TTreeNode;
begin
  ParentNode := ANode.Parent;
  while ParentNode.Level > 0 do
    ParentNode := ParentNode.Parent;

  if TObject(ParentNode.Data) is TsbxPGPKey then
    Result := TsbxPGPKey(ParentNode.Data);
end;

function TFormPgpkeys.GetDefaultUserID(Key: TsbxPGPKey): string;
begin
  if Length(Key.Username) > 0 then
    Result := Key.Username
  else
    Result := 'No name';
end;

procedure TFormPgpkeys.RedrawKeyring();
var
  i, j, k: Integer;
  Key, SubKey: TsbxPGPKey;
  KeyNode, UserNode, SubKeyNode, SigNode: TTreeNode;
  KeyManager: TsbxPGPKeyManager;
begin
  tvKeyring.Items.BeginUpdate();
  try
    ClearKeyringTree();

    for i := 0 to pgpKeyring.Keys.Count - 1 do
    begin
      Key := TsbxPGPKey(pgpKeyring.Keys.Item[i]);

      if not Key.IsSubkey then
      begin
        // Creating key node
        KeyNode := tvKeyring.Items.Add(nil, GetDefaultUserID(Key));
        KeyNode.Data := Key;
        if Pos('RSA', Key.PublicKeyAlgorithm) >= 1 then
          KeyNode.ImageIndex := 1
        else
          KeyNode.ImageIndex := 0;

        KeyNode.SelectedIndex := KeyNode.ImageIndex;

        KeyManager := TsbxPGPKeyManager.Create(nil);
        try
          KeyManager.PinnedKey := Key;
          KeyManager.ImportPinned;

          // Creating user nodes
          for j := 0 to KeyManager.Users.Count - 1 do
          begin
            UserNode := tvKeyring.Items.AddChild(KeyNode, KeyManager.Users[j].Username);
            UserNode.ImageIndex := 2;
            UserNode.SelectedIndex := 2;
            UserNode.Data := KeyManager.Users[j].Clone;

            // Creating signature nodes
            for k := 0 to KeyManager.Signatures.Count - 1 do
            begin
              if KeyManager.Signatures[k].Target = KeyManager.Users[j].Username then
              begin
                if (KeyManager.Signatures[k].Revocation) then
                begin
                  SigNode := tvKeyring.Items.AddChild(UserNode, 'Revocation');
                end
                else
                begin
                  SigNode := tvKeyring.Items.AddChild(UserNode, 'Signature');
                  SigNode.ImageIndex := 3;
                  SigNode.SelectedIndex := 3;
          	    end;

                AddSignature(SigNode, KeyManager, k);
              end;
            end;
          end;

          // Subkeys
          for j := 0 to KeyManager.Subkeys.Count - 1 do
          begin
            SubKeyNode := tvKeyring.Items.AddChild(KeyNode, KeyManager.Subkeys[j].PublicKeyAlgorithm + ' subkey');
            SubKeyNode.Data := KeyManager.Subkeys[j].Clone;

            // Creating signature nodes
            for k := 0 to KeyManager.Signatures.Count - 1 do
            begin
              if KeyManager.Signatures[k].Target = KeyManager.Subkeys[j].KeyID then
              begin
                if (KeyManager.Signatures[k].Revocation) then
                begin
                  SigNode := tvKeyring.Items.AddChild(SubKeyNode, 'Revocation');
                end
                else
                begin
                  SigNode := tvKeyring.Items.AddChild(SubKeyNode, 'Signature');
                  SigNode.ImageIndex := 3;
                  SigNode.SelectedIndex := 3;
          	    end;

                AddSignature(SigNode, KeyManager, k);
              end;
            end;
          end;
        finally
          FreeAndnil(KeyManager);
        end;
      end;
    end;
  finally
    tvKeyring.Items.EndUpdate();
  end;
end;

function TFormPgpkeys.RequestPassphrase(UserID, KeyID: string): string;
begin
  Result := '';
  with TFormPassphraseRequest.Create(Self) do
    try
      lbKeyID.Caption := UserID + ' (' + KeyID + ')';
      if ShowModal = mrOK then
        Result := edPassphrase.Text;
    finally
      Free;
    end;
end;

procedure TFormPgpkeys.DoKeyPassphraseNeeded(Sender: TObject; const KeyID: String; const UserID: String;
  MainKey: Boolean; var Passphrase: String; var Skip: Boolean);
begin
  Passphrase := RequestPassphrase(UserID, KeyID);
end;

end.








