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
unit publickeycryptof;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, SBxTypes, SBxCryptoKeyManager, SBxCertificateManager, SBxPublicKeyCrypto;

type
  TFormPublicKeyCrypto = class(TForm)
    btnGo: TButton;
    dlgOpenDialog: TOpenDialog;
    dlgSaveDialog: TSaveDialog;
    dlgOpenKey: TOpenDialog;
    gbSettings: TGroupBox;
    lblInputFIle: TLabel;
    lblPassword: TLabel;
    lblKeyFilename: TLabel;
    lblOutputFile: TLabel;
    lblInputEncoding: TLabel;
    edInputFile: TEdit;
    btnBrowseInputFile: TButton;
    btnBrowseKey: TButton;
    edKeyFile: TEdit;
    edPassphrase: TEdit;
    btnBrowseOutputFile: TButton;
    edSignatureFile: TEdit;
    cbEncoding: TComboBox;
    lblKeyContainerType: TLabel;
    cbKeyContainerType: TComboBox;
    Label10: TLabel;
    rbSign: TRadioButton;
    rbVerify: TRadioButton;
    lKeyAlg: TLabel;
    cbKeyAlg: TComboBox;
    lCurve: TLabel;
    cbCurve: TComboBox;
    procedure btnBrowseInputFileClick(Sender: TObject);
    procedure btnBrowseOutputFileClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure btnBrowseKeyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rbSignClick(Sender: TObject);
    procedure cbKeyContainerTypeChange(Sender: TObject);
    procedure cbKeyAlgChange(Sender: TObject);
  private
    procedure LoadKeyFromFile(Crypto: TSBxPublicKeyCrypto);
    procedure DoSignDetached;
    procedure DoVerifyDetached;

    procedure DoPasswordNeeded(Sender: TObject; const NeededFor: String; var Password: String; var Cancel: Boolean);
  public
    { Public declarations }
  end;

var
  FormPublicKeyCrypto: TFormPublicKeyCrypto;

implementation

{$R *.dfm}

procedure TFormPublicKeyCrypto.LoadKeyFromFile(Crypto: TSBxPublicKeyCrypto);
var
  KeyBits: integer;
  CertificateManager: TSBxCertificateManager;
  KeyManager: TSBxCryptoKeyManager;
  IV: TBytes;
begin
  if cbKeyContainerType.ItemIndex = 1 then
  begin
    CertificateManager := TSBxCertificateManager.Create(nil);
    try
      CertificateManager.ImportFromFile(edKeyFile.Text, edPassphrase.Text);

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
  end
  else
  begin
    KeyManager := TSBxCryptoKeyManager.Create(nil);
    try
      KeyManager.OnPasswordNeeded := DoPasswordNeeded;

      KeyManager.ImportFromFile(edKeyFile.Text, 1{kffAuto}, cbKeyAlg.Text, cbCurve.Text, '', 0{ktAuto});

      Crypto.Key := KeyManager.Key;
    finally
      FreeAndNil(KeyManager);
    end;
  end;
end;

procedure TFormPublicKeyCrypto.DoSignDetached;
var
  Crypto: TSBxPublicKeyCrypto;
begin
  Crypto := TSBxPublicKeyCrypto.Create(nil);
  try
    try
      case cbEncoding.ItemIndex of
        1: Crypto.OutputEncoding := cetBase64;
        2: Crypto.OutputEncoding := cetCompact;
        3: Crypto.OutputEncoding := cetJSON;
        else Crypto.OutputEncoding := cetBinary;
      end;

      // loading key
      LoadKeyFromFile(Crypto);

      // signing input data
      Crypto.SignFile(edInputFile.Text, edSignatureFile.Text, true);

      MessageDlg('The file was signed successfully', mtInformation, [mbOk], 0);
    except
      on E : Exception do
        MessageDlg(E.Message, mtError, [mbOk], 0);
    end;
  finally
    FreeAndNil(Crypto);
  end;
end;

procedure TFormPublicKeyCrypto.DoVerifyDetached;
var
  Crypto: TSBxPublicKeyCrypto;
begin
  Crypto := TSBxPublicKeyCrypto.Create(nil);
  try
    try
      case cbEncoding.ItemIndex of
        1: Crypto.InputEncoding := cetBase64;
        2: Crypto.InputEncoding := cetCompact;
        3: Crypto.InputEncoding := cetJSON;
        else Crypto.InputEncoding := cetBinary;
      end;

      // loading key
      LoadKeyFromFile(Crypto);

      // signing input data
      Crypto.VerifyDetachedFile(edInputFile.Text, edSignatureFile.Text);

      case Crypto.SignatureValidationResult of
        svtValid: MessageDlg('Verification succeeded', mtInformation, [mbOk], 0);
        svtCorrupted: MessageDlg('Verification corrupted', mtError, [mbOk], 0);
        svtFailure: MessageDlg('Verification failed', mtError, [mbOk], 0);
        else
          MessageDlg('Verification unknown', mtError, [mbOk], 0);
      end;
    except
      on E : Exception do
        MessageDlg(E.Message, mtError, [mbOk], 0);
    end;
  finally
    FreeAndNil(Crypto);
  end;
end;

procedure TFormPublicKeyCrypto.DoPasswordNeeded(Sender: TObject;
  const NeededFor: String; var Password: String; var Cancel: Boolean);
begin
  Password := edPassphrase.Text;
  Cancel := false;
end;

procedure TFormPublicKeyCrypto.btnBrowseInputFileClick(Sender: TObject);
begin
  if dlgOpenDialog.Execute then
    edInputFile.Text := dlgOpenDialog.FileName;
end;

procedure TFormPublicKeyCrypto.btnBrowseOutputFileClick(Sender: TObject);
begin
  if rbSign.Checked then
  begin
    if dlgSaveDialog.Execute then
      edSignatureFile.Text := dlgSaveDialog.FileName;
  end
  else
  begin
    if dlgOpenDialog.Execute then
      edSignatureFile.Text := dlgOpenDialog.FileName;
  end;
end;

procedure TFormPublicKeyCrypto.btnBrowseKeyClick(Sender: TObject);
begin
  if dlgOpenKey.Execute then
    edKeyFile.Text := dlgOpenKey.FileName;
end;

procedure TFormPublicKeyCrypto.btnGoClick(Sender: TObject);
begin
  if not FileExists(edInputFile.Text) then
    MessageDlg('Source file not found', mtError, [mbOk], 0)
  else if edSignatureFile.Text = '' then
    MessageDlg('Please provide a valid name for the signature file', mtError, [mbOk], 0)
  else if not FileExists(edKeyFile.Text) then
    MessageDlg('Key container file not found', mtError, [mbOk], 0)
  else
  begin
    if rbSign.Checked then
      DoSignDetached
    else
    begin
      if not FileExists(edSignatureFile.Text) then
        MessageDlg('Signature file not found', mtError, [mbOk], 0)
      else
        DoVerifyDetached;
    end;
  end;
end;

procedure TFormPublicKeyCrypto.cbKeyAlgChange(Sender: TObject);
begin
  if cbKeyAlg.Text = 'EC' then
  begin
    lCurve.Enabled := cbKeyAlg.Enabled;
    cbCurve.Enabled := cbKeyAlg.Enabled;
  end
  else
  begin
    lCurve.Enabled := false;
    cbCurve.Enabled := false;
    cbCurve.ItemIndex := 0;
  end;
end;

procedure TFormPublicKeyCrypto.cbKeyContainerTypeChange(Sender: TObject);
begin
  if cbKeyContainerType.ItemIndex = 0 then
  begin
    lKeyAlg.Enabled := true;
    cbKeyAlg.Enabled := true;
  end
  else
  begin
    lKeyAlg.Enabled := false;
    cbKeyAlg.Enabled := false;
    cbKeyAlg.ItemIndex := 0;
  end;

  cbKeyAlgChange(cbKeyAlg);
end;

procedure TFormPublicKeyCrypto.FormCreate(Sender: TObject);
begin
  rbSignClick(rbSign);
  cbKeyContainerTypeChange(cbKeyContainerType);
end;

procedure TFormPublicKeyCrypto.rbSignClick(Sender: TObject);
begin
  if rbSign.Checked then
  begin
    lblOutputFile.Caption := 'Output filename:';
    btnGo.Caption := 'Sign';
  end
  else
  begin
    lblOutputFile.Caption := 'Signature filename:';
    btnGo.Caption := 'Verify';
  end;
end;

end.

