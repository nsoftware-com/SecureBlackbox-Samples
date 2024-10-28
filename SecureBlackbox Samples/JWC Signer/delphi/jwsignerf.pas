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
unit jwsignerf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SBxTypes, SBxCertificateManager, SBxCryptoKeyManager, SBxPublicKeyCrypto;

type
  TFormJWSigner = class(TForm)
    edInputStr: TEdit;
    Label1: TLabel;
    edSignatureStr: TEdit;
    bSign: TButton;
    bVerify: TButton;
    edPassword: TEdit;
    Label4: TLabel;
    Label2: TLabel;
    Label10: TLabel;
    cbCompact: TCheckBox;
    lblKeyFilename: TLabel;
    edCertFile: TEdit;
    btnBrowse: TButton;
    dlgOpen: TOpenDialog;
    procedure bSignClick(Sender: TObject);
    procedure bVerifyClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
  private
    { Private declarations }
    procedure LoadKeyFromCertFile(Crypto: TSBxPublicKeyCrypto);
  public
    { Public declarations }
  end;

var
  FormJWSigner: TFormJWSigner;

implementation

{$R *.DFM}

procedure TFormJWSigner.btnBrowseClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edCertFile.Text := dlgOpen.FileName;
end;

procedure TFormJWSigner.LoadKeyFromCertFile(Crypto: TSBxPublicKeyCrypto);
var
  KeyBits: integer;
  CertificateManager: TSBxCertificateManager;
  KeyManager: TSBxCryptoKeyManager;
  IV: TBytes;
begin
  CertificateManager := TSBxCertificateManager.Create(nil);
  try
    CertificateManager.ImportFromFile(edCertFile.Text, edPassword.Text);

    KeyManager := TSBxCryptoKeyManager.Create(nil);
    try
      KeyManager.Certificate := CertificateManager.Certificate;

      KeyManager.ImportFromCert;

      Crypto.Key := KeyManager.Key;
    finally
      FreeAndNil(KeyManager);
    end;
  finally
    FreeAndNil(CertificateManager);
  end;
end;

procedure TFormJWSigner.bSignClick(Sender: TObject);
var
  Crypto: TSBxPublicKeyCrypto;
  InBuf, OutBuf: TBytes;
begin
  Crypto := TSBxPublicKeyCrypto.Create(nil);
  try
    if cbCompact.Checked then
      Crypto.OutputEncoding := cetCompact
    else
      Crypto.OutputEncoding := cetJSON;

    try
      // loading key
      LoadKeyFromCertFile(Crypto);

      InBuf := TEncoding.ANSI.GetBytes(edInputStr.Text);

      OutBuf := Crypto.Sign(InBuf, true);

      edSignatureStr.Text := TEncoding.ANSI.GetString(OutBuf);
    except
      on Ex : Exception do
        ShowMessage('Signing error: ' + Ex.Message);
    end;
  finally
    FreeAndNil(Crypto);
  end;
end;

procedure TFormJWSigner.bVerifyClick(Sender: TObject);
var
  Crypto: TSBxPublicKeyCrypto;
  InBuf, SigBuf: TBytes;
begin
  Crypto := TSBxPublicKeyCrypto.Create(nil);
  try
    if cbCompact.Checked then
      Crypto.InputEncoding := cetCompact
    else
      Crypto.InputEncoding := cetJSON;

    try
      // loading key
      LoadKeyFromCertFile(Crypto);

      InBuf := TEncoding.ANSI.GetBytes(edInputStr.Text);
      SigBuf := TEncoding.ANSI.GetBytes(edSignatureStr.Text);

      Crypto.VerifyDetached(InBuf, SigBuf);

      case Crypto.SignatureValidationResult of
        svtValid: MessageDlg('Verification succeeded', mtInformation, [mbOk], 0);
        svtCorrupted: MessageDlg('Verification corrupted', mtError, [mbOk], 0);
        svtFailure: MessageDlg('Verification failed', mtError, [mbOk], 0);
        else
          MessageDlg('Verification unknown', mtError, [mbOk], 0);
      end;
    except
      on Ex : Exception do
        ShowMessage('Verifying error: ' + Ex.Message);
    end;
  finally
    FreeAndNil(Crypto);
  end;
end;
end.



