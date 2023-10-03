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
unit hashfunctionf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SBxTypes, SBxCryptoKeyManager, SBxHashFunction;

type
  TFormHashFunction = class(TForm)
    edInputStr: TEdit;
    Label1: TLabel;
    edHashStr: TEdit;
    bHash: TButton;
    edPassword: TEdit;
    Label4: TLabel;
    Label2: TLabel;
    cbEncoding: TComboBox;
    Label3: TLabel;
    Label10: TLabel;
    procedure bHashClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormHashFunction: TFormHashFunction;

implementation

{$R *.DFM}

procedure PasswordToKey(const Pass: string; Hash: TSBxHashFunction);
var
  KeyManager: TSBxCryptoKeyManager;
begin
  KeyManager := TSBxCryptoKeyManager.Create(nil);
  try
    KeyManager.DeriveKey(256, Pass, '');

    Hash.Key := KeyManager.Key;
  finally
    FreeAndNil(KeyManager);
  end;
end;

procedure TFormHashFunction.bHashClick(Sender: TObject);
var
  Hash: TSBxHashFunction;
  InBuf, OutBuf: TBytes;
begin
  Hash := TSBxHashFunction.Create(nil);
  try
    Hash.Algorithm := 'SHA256';

    case cbEncoding.ItemIndex of
      1: Hash.OutputEncoding := cetBase64;
      2: Hash.OutputEncoding := cetCompact;
      3: Hash.OutputEncoding := cetJSON;
      else Hash.OutputEncoding := cetBinary;
    end;

    try
      if edPassword.Text <> '' then
        PasswordToKey(edPassword.Text, Hash);

      InBuf := TEncoding.ANSI.GetBytes(edInputStr.Text);

      OutBuf := Hash.Hash(InBuf);

      edHashStr.Text := TEncoding.ANSI.GetString(OutBuf);
    except
      on Ex : Exception do
        ShowMessage('Hash error: ' + Ex.Message);
    end;
  finally
    FreeAndNil(Hash);
  end;
end;

end.


