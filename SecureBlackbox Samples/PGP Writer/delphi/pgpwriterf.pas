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
unit pgpwriterf;

{$IFDEF VER180}
    {$DEFINE BUILDER_USED}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  SBxTypes, SBxCore, SBxPGpKeyring, SBxPGPWriter;

type
  TFormPgpwriter = class(TForm)
    lbInputFileName: TLabel;
    btnBrowseInputFile: TButton;
    btnEncrypt: TButton;
    Label1: TLabel;
    editOutputFile: TEdit;
    btnBrowseOutFile: TButton;
    editInputFile: TEdit;
    Label10: TLabel;
    GroupBox1: TGroupBox;
    dlgOpenDialog: TOpenDialog;
    dlgSaveDialog: TSaveDialog;
    cbEncryptKeySelect: TComboBox;
    cbSignKeySelect: TComboBox;
    btnBrowseKeyringFile: TButton;
    lbPublicKeyList: TLabel;
    lbSecretKeyList: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure btnEncryptClick(Sender: TObject);
    procedure btnBrowseKeyringFileClick(Sender: TObject);
    procedure btnBrowseOutputFileClick(Sender: TObject);
    procedure btnBrowseInputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    pgpKeyring: TsbxPGPKeyring;
    pgpWriter: TsbxPGPWriter;

    procedure LoadKeyring;
    procedure PopulateKeysList;
    function RequestKeyPassphrase(const KeyID: string; const UserID: string;
      MainKey: boolean; var Skip: Boolean): string;

    procedure DoKeyPassphraseNeeded(Sender: TObject; const KeyID: string; const UserID: string;
      MainKey: boolean; var Passphrase: string; var Skip: boolean);
  public
    { Public declarations }
    procedure EncryptAndSign(const strInputFilename: string; const strOutputFilename: string;
      EncryptingKey: TsbxPGPKey; SigningKey: TsbxPGPKey);
  end;

var
  FormPgpwriter: TFormPgpwriter;

implementation


{$R *.dfm}

uses keyringloadf, passphraserequestf;

procedure TFormPgpwriter.EncryptAndSign(const strInputFilename: string; const strOutputFilename: string;
  EncryptingKey: TsbxPGPKey; SigningKey: TsbxPGPKey);
begin
  try
    pgpWriter.Armor := true;
    pgpWriter.ArmorHeaders := 'Version: OpenPGPBlackbox';
    pgpWriter.ArmorBoundary := 'PGP MESSAGE';
    pgpWriter.EncryptingKeys.Clear;
    pgpWriter.EncryptingKeys.Add(EncryptingKey);
    pgpWriter.SigningKeys.Clear;
    pgpWriter.SigningKeys.Add(SigningKey);

    // encrypt with public key
    pgpWriter.Timestamp := DateTimeToStr(Now);

    pgpWriter.Filename := ExtractFileName(strInputFilename);

    pgpWriter.InputFile := strInputFilename;
    pgpWriter.OutputFile := strOutputFilename;

    pgpWriter.EncryptAndSign();

    MessageDlg('The files were encrypted and signed successfully', mtInformation, [mbOk], 0);
    Close;
  except
    on E : Exception do
      MessageDlg('Failed to encrypt and sign file: ' + E.Message, mtError, [mbOk], 0);
  end;
end;

procedure TFormPgpwriter.DoKeyPassphraseNeeded(Sender: TObject; const KeyID: string;
  const UserID: string; MainKey: boolean; var Passphrase: string; var Skip: boolean);
begin
  Passphrase := RequestKeyPassphrase(KeyID, UserID, MainKey, Skip);
end;

procedure TFormPgpwriter.FormCreate(Sender: TObject);
begin
  pgpKeyring := TsbxPGPKeyring.Create(nil);

  pgpWriter := TsbxPGPWriter.Create(nil);
  pgpWriter.OnKeyPassphraseNeeded := DoKeyPassphraseNeeded;
end;

procedure TFormPgpwriter.FormDestroy(Sender: TObject);
begin
  FreeAndNil(pgpKeyring);
  FreeAndNil(pgpWriter);
end;

procedure TFormPgpwriter.LoadKeyring;
begin
  if FormKeyringload.ShowModal = mrOK then
  begin
    try
      pgpKeyring.Reset();

      pgpKeyring.ImportFromFile(FormKeyringload.editPubKeyring.Text);
      pgpKeyring.ImportFromFile(FormKeyringload.editSecKeyring.Text);

      PopulateKeysList;
    except
      on E : Exception do
        MessageDlg('Failed to load keyring: ' + E.Message, mtError, [mbOk], 0);
    end;
  end;
end;

function TFormPgpwriter.RequestKeyPassphrase(const KeyID: string; const UserID: string;
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

procedure TFormPgpwriter.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormPgpwriter.btnEncryptClick(Sender: TObject);
begin
  if not FileExists(editInputFile.Text) then
    MessageDlg('Input file does not exist', mtError, [mbOk], 0)
  else if editOutputFile.Text = '' then
    MessageDlg('Please select output file', mtError, [mbOk], 0)
  else if pgpKeyring.Keys.Count = 0 then
    MessageDlg('Your keyring does not contain public keys.' +
      'You will not be able to encrypt file.'#13#10 +
      'Please, select another keyring file.', mtError, [mbOk], 0)
  else if cbSignKeySelect.Items.Count = 0 then
    MessageDlg('Your keyring does not contain private keys.' +
      'You will not be able to sign file.'#13#10 +
      'Please, select another keyring file.', mtError, [mbOk], 0)
  else if cbEncryptKeySelect.ItemIndex = -1 then
    MessageDlg('Please select the encryption key', mtError, [mbOk], 0)
  else if cbSignKeySelect.ItemIndex = -1 then
    MessageDlg('Please select the signing key', mtError, [mbOk], 0)
  else
  begin
    EncryptAndSign(editInputFile.Text, editOutputFile.Text, TsbxPGPKey(cbEncryptKeySelect.Items.Objects[cbEncryptKeySelect.ItemIndex]),
      TsbxPGPKey(cbSignKeySelect.Items.Objects[cbSignKeySelect.ItemIndex]));
  end;
end;

procedure TFormPgpwriter.btnBrowseKeyringFileClick(Sender: TObject);
begin
  LoadKeyring;
end;

procedure TFormPgpwriter.btnBrowseInputFileClick(Sender: TObject);
begin
  dlgOpenDialog.Filter := '';
  dlgOpenDialog.Title := 'Please, select file';
  if dlgOpenDialog.Execute then
    editInputFile.Text := dlgOpenDialog.FileName;
end;

procedure TFormPgpwriter.btnBrowseOutputFileClick(Sender: TObject);
begin
  dlgSaveDialog.Filter := '';
  dlgSaveDialog.Title := 'Please, select file';
  if dlgSaveDialog.Execute then
    editOutputFile.Text := dlgSaveDialog.FileName;
end;

function GetUserFriendlyKeyName(Key : TsbxPGPKey): string;
begin
  if Key.Username <> '' then
    Result := Key.Username
  else
    Result := 'No name';

  Result := Result + ' [0x' + Key.KeyID + ']';
end;

procedure TFormPgpwriter.PopulateKeysList;
var
  i: integer;
  Mods : string;
begin
  cbEncryptKeySelect.Clear;
  for I := 0 to pgpKeyring.Keys.Count - 1 do
  begin
    if pgpKeyring.Keys.Item[i].IsSubkey then
      Mods := '[SUB] '
    else
      Mods := '[PRI] ';

    cbEncryptKeySelect.Items.AddObject(Mods + GetUserFriendlyKeyName(pgpKeyring.Keys.Item[i]), pgpKeyring.Keys.Item[i]);
  end;

  cbSignKeySelect.Clear;
  for I := 0 to pgpKeyring.Keys.Count - 1 do
  begin
    if pgpKeyring.Keys.Item[i].IsSecret then
    begin
      if pgpKeyring.Keys.Item[i].IsSubkey then
        Mods := '[SUB] '
      else
        Mods := '[PRI] ';

      cbSignKeySelect.Items.AddObject(Mods + GetUserFriendlyKeyName(pgpKeyring.Keys.Item[i]), pgpKeyring.Keys.Item[i]);
    end;
  end;
end;

end.





