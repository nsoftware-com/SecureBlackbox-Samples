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
unit symmetriccryptof;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SBxTypes, SBxCryptoKeyManager, SBxSymmetricCrypto;

type
  TFormSymmetricCrypto = class(TForm)
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
    cbEncoding: TComboBox;
    Label3: TLabel;
    Label10: TLabel;
    procedure bbEncryptClick(Sender: TObject);
    procedure bbDecryptClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSymmetricCrypto: TFormSymmetricCrypto;

implementation

{$R *.DFM}

procedure PasswordToKey(const Pass: string; Crypto: TSBxSymmetricCrypto);
var
  KeyBits: integer;
  KeyManager: TSBxCryptoKeyManager;
  IV: TBytes;
begin
  KeyManager := TSBxCryptoKeyManager.Create(nil);
  try
    KeyBits := 256;

    KeyManager.DeriveKey(KeyBits, Pass, ''); // derive 256-bit key from password

    SetLength(IV, 16);
    FillChar(IV[0], Length(IV), 0);
    KeyManager.Key.IV := IV; // set 128-bit initialization vector

    Crypto.Key := KeyManager.Key;
  finally
    FreeAndNil(KeyManager);
  end;
end;

procedure TFormSymmetricCrypto.bbEncryptClick(Sender: TObject);
var
  Crypto: TSBxSymmetricCrypto;
  InBuf, OutBuf: TBytes;
begin
  Crypto := TSBxSymmetricCrypto.Create(nil);
  try
    Crypto.EncryptionAlgorithm := 'AES256';

    case cbEncoding.ItemIndex of
      1: Crypto.OutputEncoding := cetBase64;
      2: Crypto.OutputEncoding := cetCompact;
      3: Crypto.OutputEncoding := cetJSON;
      else Crypto.OutputEncoding := cetBinary;
    end;

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

procedure TFormSymmetricCrypto.bbDecryptClick(Sender: TObject);
var
  Crypto: TSBxSymmetricCrypto;
  InBuf, OutBuf: TBytes;
begin
  Crypto := TSBxSymmetricCrypto.Create(nil);
  try
    Crypto.EncryptionAlgorithm := 'AES256';

    case cbEncoding.ItemIndex of
      1: Crypto.InputEncoding := cetBase64;
      2: Crypto.InputEncoding := cetCompact;
      3: Crypto.InputEncoding := cetJSON;
      else Crypto.InputEncoding := cetBinary;
    end;

    try
      // Key from password
      PasswordToKey(edPassword.Text, Crypto);

      InBuf := TEncoding.ANSI.GetBytes(edEncryptedStr.Text);

      OutBuf := Crypto.Decrypt(InBuf);

      edDecryptedStr.Text := TEncoding.ANSI.GetString(OutBuf);
    except
      on Ex : Exception do
        ShowMessage('Encryption error: ' + Ex.Message);
    end;
  finally
    FreeAndNil(Crypto);
  end;
end;

end.

