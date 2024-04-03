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
unit jwencryptionf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SBxTypes, SBxCryptoKeyManager, SBxSymmetricCrypto;

type
  TFormJWEncryption = class(TForm)
    edInputStr: TEdit;
    Label1: TLabel;
    edEncryptedStr: TEdit;
    bbEncrypt: TButton;
    bbDecrypt: TButton;
    edDecryptedStr: TEdit;
    edPassword: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label2: TLabel;
    Label10: TLabel;
    cbCompact: TCheckBox;
    procedure bbEncryptClick(Sender: TObject);
    procedure bbDecryptClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormJWEncryption: TFormJWEncryption;

implementation

{$R *.DFM}

procedure PasswordToKey(const Pass: string; Crypto: TSBxSymmetricCrypto);
var
  KeyManager: TSBxCryptoKeyManager;
  IV: TBytes;
begin
  KeyManager := TSBxCryptoKeyManager.Create(nil);
  try
    KeyManager.DeriveKey(256, Pass, '');

    SetLength(IV, 16);
    FillChar(IV[0], Length(IV), 0);
    KeyManager.Key.IV := IV;

    Crypto.Key := KeyManager.Key;
  finally
    FreeAndNil(KeyManager);
  end;
end;

procedure TFormJWEncryption.bbEncryptClick(Sender: TObject);
var
  Crypto: TSBxSymmetricCrypto;
  InBuf, OutBuf: TBytes;
begin
  Crypto := TSBxSymmetricCrypto.Create(nil);
  try
    Crypto.EncryptionAlgorithm := 'AES256';

    if cbCompact.Checked then
      Crypto.OutputEncoding := cetCompact
    else
      Crypto.OutputEncoding := cetJSON;

    try
      // Key from password
      PasswordToKey(edPassword.Text, Crypto);

      InBuf := TEncoding.ANSI.GetBytes(edInputStr.Text);

      OutBuf := Crypto.Encrypt(InBuf);

      edEncryptedStr.Text := TEncoding.ANSI.GetString(OutBuf);
    except
      on Ex : Exception do
        ShowMessage('Encryption error: ' + Ex.Message);
    end;
  finally
    FreeAndNil(Crypto);
  end;
end;

procedure TFormJWEncryption.bbDecryptClick(Sender: TObject);
var
  Crypto: TSBxSymmetricCrypto;
  InBuf, OutBuf: TBytes;
begin
  Crypto := TSBxSymmetricCrypto.Create(nil);
  try
    Crypto.EncryptionAlgorithm := 'AES256';

    if cbCompact.Checked then
      Crypto.InputEncoding := cetCompact
    else
      Crypto.InputEncoding := cetJSON;

    try
      // Key from password
      PasswordToKey(edPassword.Text, Crypto);

      InBuf := TEncoding.ANSI.GetBytes(edEncryptedStr.Text);

      OutBuf := Crypto.Decrypt(InBuf);

      edDecryptedStr.Text := TEncoding.ANSI.GetString(OutBuf);
    except
      on Ex : Exception do
        ShowMessage('Decryption error: ' + Ex.Message);
    end;
  finally
    FreeAndNil(Crypto);
  end;
end;

end.


