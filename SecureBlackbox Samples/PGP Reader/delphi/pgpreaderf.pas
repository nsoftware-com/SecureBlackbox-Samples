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
unit pgpreaderf;

{$IFDEF VER180}
    {$DEFINE BUILDER_USED}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  SBxTypes, SBxCore, SBxPGPReader, SBxPGPKeyring;

type
  TFormPgpreader = class(TForm)
    lbInputFileName: TLabel;
    editInputFile: TEdit;
    btnBrowseInputFile: TButton;
    btnDecrypt: TButton;
    dlgOpenDialog: TOpenDialog;
    Label1: TLabel;
    editOutputFile: TEdit;
    btnBrowseOutFile: TButton;
    dlgSaveDialog: TSaveDialog;
    Label10: TLabel;
    GroupBox1: TGroupBox;
    lbDecryptKeyList: TLabel;
    lbVerifyKeyList: TLabel;
    btnBrowseKeyringFile: TButton;
    cbDecryptKeySelect: TComboBox;
    cbVerifyKeySelect: TComboBox;
    cbAutoKeySelect: TCheckBox;
    procedure btnCloseClick(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
    procedure btnBrowseKeyringFileClick(Sender: TObject);
    procedure btnBrowseOutputFileClick(Sender: TObject);
    procedure btnBrowseInputFileClick(Sender: TObject);
    procedure cbAutoKeySelectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    pgpReader: TsbxPGPReader;
    pgpKeyring: TsbxPGPKeyring;

    procedure DoKeyPassphraseNeeded(Sender: TObject; const KeyID: string;
      const UserID: string; MainKey: boolean; var Passphrase: string; var Skip: boolean);
    procedure DoMultipleFilesFound(Sender: TObject; const TarFilename : string; var Proceed: boolean);

    procedure LoadKeyring;
    procedure PopulateKeysList;
    function RequestKeyPassphrase(const KeyID: string; const UserID: string;
      MainKey: boolean; var Skip: Boolean): string;

    procedure DecryptAndVerify(const strInputFilename: string; const strOutputFilename: string; Keyring : TsbxPGPKeyring); overload;
    procedure DecryptAndVerify(const strInputFilename: string; const strOutputFilename: string;
      DecryptKey: TsbxPGPKey; VerifyKey: TsbxPGPKey); overload;
    procedure ShowSignatures();
  public
    { Public declarations }
  end;

var
  FormPgpreader: TFormPgpreader;

implementation

{$R *.dfm}

uses keyringloadf, passphraserequestf, signaturesf;

procedure TFormPgpreader.DecryptAndVerify(const strInputFilename: string; const strOutputFilename: string;
  Keyring : TsbxPGPKeyring);
var
  i: integer;
begin
  try
    pgpReader.DecryptingKeys.Clear;
    for i := 0 to Keyring.SecretKeys.Count - 1 do
      pgpReader.DecryptingKeys.Add(Keyring.SecretKeys.Item[i]);
    pgpReader.VerifyingKeys.Clear;
    for i := 0 to Keyring.PublicKeys.Count - 1 do
      pgpReader.VerifyingKeys.Add(Keyring.PublicKeys.Item[i]);

    pgpReader.InputFile := strInputFilename;
    pgpReader.OutputFile := strOutputFilename;

    pgpReader.DecryptAndVerify();

    ShowSignatures;

    MessageDlg('The file was decrypted successfully', mtInformation, [mbOk], 0);
    Close;
  except
    on E : Exception do
      MessageDlg('Failed to decrypt and verify file: ' + E.Message, mtError, [mbOk], 0);
  end;
end;

procedure TFormPgpreader.DecryptAndVerify(const strInputFilename: string; const strOutputFilename: string;
  DecryptKey: TsbxPGPKey; VerifyKey: TsbxPGPKey);
begin
  try
    pgpReader.DecryptingKeys.Clear;
    pgpReader.DecryptingKeys.Add(DecryptKey);
    pgpReader.VerifyingKeys.Clear;
    pgpReader.VerifyingKeys.Add(VerifyKey);

    pgpReader.InputFile := strInputFilename;
    pgpReader.OutputFile := strOutputFilename;

    pgpReader.DecryptAndVerify();

    ShowSignatures;

    MessageDlg('The file was decrypted successfully', mtInformation, [mbOk], 0);
    Close;
  except
    on E : Exception do
      MessageDlg('Failed to decrypt and verify file: ' + E.Message, mtError, [mbOk], 0);
  end;
end;

procedure TFormPgpreader.ShowSignatures();
begin
  with TFormSignatures.Create(Self) do
    try
      Init(pgpReader.Signatures, pgpKeyring);
      ShowModal;
    finally
      Free;
    end;
end;

procedure TFormPgpreader.DoKeyPassphraseNeeded(Sender: TObject; const KeyID: string;
  const UserID: string; MainKey: boolean; var Passphrase: string; var Skip: boolean);
begin
  Passphrase := RequestKeyPassphrase(KeyID, UserID, MainKey, Skip);
end;

function TFormPgpreader.RequestKeyPassphrase(const KeyID: string; const UserID: string;
  MainKey: boolean; var Skip: Boolean): string;
var
  Username: string;
begin
  Skip := False;
  Result := '';
  with TFormPassphraserequest.Create(Self) do
    try
      Username := UserID;
      if not MainKey then
        Username := Username + ' Subkey';

      lbPrompt.Caption := 'Passphrase is needed for secret key:';
      lbKeyID.Caption := UserName + ' (ID=0x' + KeyID + ')';

      if ShowModal = mrOK then
        Result := edPassphrase.Text
      else
        Skip := True;
    finally
      Free;
    end;
end;

procedure TFormPgpreader.DoMultipleFilesFound(Sender: TObject; const TarFilename: string; var Proceed: boolean);
begin
  Proceed := Application.MessageBox(PChar(TarFilename + ' file found inside the encrypted file.'#13#10 +
    'Do you want to extract it after decryption or decrypt as is?'),
    'Question', MB_YESNO) = IDYES;
end;

procedure TFormPgpreader.FormCreate(Sender: TObject);
begin
  pgpKeyring := TsbxPGPKeyring.Create(nil);

  pgpReader := TsbxPGPReader.Create(nil);
  pgpReader.OnKeyPassphraseNeeded := DoKeyPassphraseNeeded;
  pgpReader.OnMultipleFilesFound := DoMultipleFilesFound;
end;

procedure TFormPgpreader.FormDestroy(Sender: TObject);
begin
  FreeAndNil(pgpReader);
  FreeAndNil(pgpKeyring);
end;

// allow use to select public and secret keyrings
procedure TFormPgpreader.LoadKeyring;
begin
  if FormKeyringload.ShowModal = mrOK then
  begin
    try
      if pgpKeyring.Opened then
        pgpKeyring.Close();

      pgpKeyring.Load(FormKeyringload.editPubKeyring.Text, FormKeyringload.editSecKeyring.Text);

      PopulateKeysList;
    except
      on E : Exception do
        MessageDlg('Failed to load keyring: ' + E.Message, mtError, [mbOk], 0);
    end;
  end;
end;

function GetUserFriendlyKeyName(Key : TsbxPGPKey): string;
begin
  if Key.Username <> '' then
    Result := Key.Username
  else
    Result := 'No name';

  Result := Result + ' [0x' + Key.KeyID + ']';
end;

procedure TFormPgpreader.PopulateKeysList;
var
  i: integer;
  Mods : string;
begin
  cbVerifyKeySelect.Clear;
  for I := 0 to pgpKeyring.PublicKeys.Count - 1 do
  begin
    if pgpKeyring.PublicKeys.Item[i].IsSubkey then
      Mods := '[SUB] '
    else
      Mods := '[PRI] ';

    cbVerifyKeySelect.Items.AddObject(Mods + GetUserFriendlyKeyName(TsbxPGPKey(pgpKeyring.PublicKeys.Item[i])), pgpKeyring.PublicKeys.Item[i]);
  end;

  cbDecryptKeySelect.Clear;
  for I := 0 to pgpKeyring.SecretKeys.Count - 1 do
  begin
    if pgpKeyring.SecretKeys.Item[i].IsSubkey then
      Mods := '[SUB] '
    else
      Mods := '[PRI] ';

    cbDecryptKeySelect.Items.AddObject(Mods + GetUserFriendlyKeyName(TsbxPGPKey(pgpKeyring.SecretKeys.Item[i])), pgpKeyring.SecretKeys.Item[i]);
  end;
end;

procedure TFormPgpreader.btnCloseClick(Sender: TObject);
begin
  Close;
end; 

procedure TFormPgpreader.btnDecryptClick(Sender: TObject);
begin
  if cbAutoKeySelect.Checked then
  begin
    if not FileExists(editInputFile.Text) then
      MessageDlg('Source file not found', mtError, [mbOk], 0)
    else if editOutputFile.Text = '' then
      MessageDlg('Please select output file', mtError, [mbOk], 0)
    else if pgpKeyring.SecretKeys.Count = 0 then
      MessageDlg('Your keyring does not contain private keys.' +
        'You will not be able to decrypt encrypted files.'#13#10 +
        'Please, select another keyring file.', mtError, [mbOk], 0)
    else if pgpKeyring.PublicKeys.Count = 0 then
      MessageDlg('Your keyring does not contain public keys.' +
         'You will not be able to verify files.'#13#10 +
         'Please, select another keyring file.', mtError, [mbOk], 0)
    else
      DecryptAndVerify(editInputFile.Text, editOutputFile.Text, pgpKeyring);
  end
  else
  begin
    if not FileExists(editInputFile.Text) then
      MessageDlg('Source file not found', mtError, [mbOk], 0)
    else if editOutputFile.Text = '' then
      MessageDlg('Please select output file', mtError, [mbOk], 0)
    else if pgpKeyring.SecretKeys.Count = 0 then
      MessageDlg('Your keyring does not contain private keys.' +
        'You will not be able to decrypt encrypted files.'#13#10 +
        'Please, select another keyring file.', mtError, [mbOk], 0)
    else if pgpKeyring.PublicKeys.Count = 0 then
      MessageDlg('Your keyring does not contain public keys.' +
        'You will not be able to verify files.'#13#10 +
        'Please, select another keyring file.', mtError, [mbOk], 0)
    else if cbDecryptKeySelect.ItemIndex = -1 then
      MessageDlg('Please select decryption key', mtError, [mbOk], 0)
    else if cbVerifyKeySelect.ItemIndex = -1 then
      MessageDlg('Please select verifing key', mtError, [mbOk], 0)
    else
    begin
      DecryptAndVerify(editInputFile.Text, editOutputFile.Text,
        TsbxPGPKey(cbDecryptKeySelect.Items.Objects[cbDecryptKeySelect.ItemIndex]),
        TsbxPGPKey(cbVerifyKeySelect.Items.Objects[cbVerifyKeySelect.ItemIndex]));
    end;
  end;
end;

procedure TFormPgpreader.btnBrowseKeyringFileClick(Sender: TObject);
begin
  LoadKeyring;
end;

procedure TFormPgpreader.btnBrowseInputFileClick(Sender: TObject);
begin
  dlgOpenDialog.Filter := '';
  dlgOpenDialog.Title := 'Please, select file';
  if dlgOpenDialog.Execute then
    editInputFile.Text := dlgOpenDialog.FileName;
end;

procedure TFormPgpreader.btnBrowseOutputFileClick(Sender: TObject);
begin
  dlgSaveDialog.Filter := '';
  dlgSaveDialog.Title := 'Please, select file';
  if dlgSaveDialog.Execute then
    editOutputFile.Text := dlgSaveDialog.FileName;
end;

procedure TFormPgpreader.cbAutoKeySelectClick(Sender: TObject);
begin
  if not cbAutoKeySelect.Checked then
  begin
    lbDecryptKeyList.Enabled := True;
    lbVerifyKeyList.Enabled := True;
    cbDecryptKeySelect.Enabled := True;
    cbVerifyKeySelect.Enabled := True;
  end
  else
  begin
    lbDecryptKeyList.Enabled := False;
    lbVerifyKeyList.Enabled := False;
    cbDecryptKeySelect.Enabled := False;
    cbVerifyKeySelect.Enabled := False;
  end;
end;

end.




