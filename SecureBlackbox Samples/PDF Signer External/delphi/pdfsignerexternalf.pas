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
unit pdfsignerexternalf;

interface

uses
  SysUtils, Variants, Classes, Graphics, StrUtils,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxCertificateManager, SBxCryptoKeyManager, SBxPublicKeyCrypto, SBxPDFSigner;

type
  TFormPdfsignerexternal = class(TForm)
    Label10: TLabel;
    lbInputFile: TLabel;
    sbBrowseInputFile: TSpeedButton;
    Label1: TLabel;
    sbOutputFile: TSpeedButton;
    edInputFile: TEdit;
    edOutputFile: TEdit;
    GroupBox6: TGroupBox;
    Label3: TLabel;
    sbSignCertFile: TSpeedButton;
    Label5: TLabel;
    Label4: TLabel;
    edSigningCertificate: TEdit;
    edCertPassword: TEdit;
    cbLevel: TComboBox;
    cbVisible: TCheckBox;
    btnSign: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    Label2: TLabel;
    edSigningKey: TEdit;
    sbSignKeyFile: TSpeedButton;
    procedure sbBrowseInputFileClick(Sender: TObject);
    procedure sbOutputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure sbSignCertFileClick(Sender: TObject);
    procedure sbSignKeyFileClick(Sender: TObject);
  private
    { Private declarations }
    FSigner: TsbxPDFSigner;

    procedure HandleExternalSign(Sender: TObject; const OperationId: String;
      const HashAlgorithm: String; const Pars: String; const Data: String; var SignedData: String);
  public
    { Public declarations }
  end;

var
  FormPdfsignerexternal: TFormPdfsignerexternal;

implementation

{$R *.dfm}

function StringToBinary(const Value: String): TBytes;
  function ConvertChar(Ch: byte) : byte;
  begin
    Result := 0;

    if (Ch >= $30) and (Ch <= $39) then
      Result := Ch - $30
    else if (Ch >= $41) and (Ch <= $46) then
      Result := Ch - 55
    else if (Ch >= $61) and (Ch <= $66) then
      Result := Ch - 87;
  end;

var
  Buf: TBytes;
  InPtr, OutPtr: ^byte;
  BytesLeft: integer;
  i, Size: integer;
begin
  Size := Length(Value) shr 1;
  SetLength(Result, Size);

  SetLength(Buf, Length(Value));
  for i := 0 to Length(Buf) - 1 do
    Buf[i] := Byte(Ord(Value[i + 1]));
  InPtr := @Buf[0];

  OutPtr := @Result[0];
  BytesLeft := Length(Value);
  while BytesLeft > 0 do
  begin
    OutPtr^ := (ConvertChar(InPtr^) shl 4) or ConvertChar(PByteArray(InPtr)[1]);
    Inc(InPtr, 2);
    Inc(OutPtr);
    Dec(BytesLeft, 2);
  end;
end;

function LeftPad(Value: string; Len: integer): string;
var
  i: integer;
begin
  Result := Value;
  while Length(Result) < Len do
    Result := '0' + Result;
end;

function BinaryToString(const Buffer : TBytes): string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to Length(Buffer) - 1 do
    Result := Result + LeftPad(IntToHex(Buffer[i], 1), 2);
end;

procedure TFormPdfsignerexternal.btnSignClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
begin
  FSigner.InputFile := edInputFile.Text;
  FSigner.OutputFile := edOutputFile.Text;

  CertificateManager := TsbxCertificateManager.Create(nil);
  try
    try
      CertificateManager.ImportFromFile(edSigningCertificate.Text, edCertPassword.Text);

      FSigner.SigningCertificate := CertificateManager.Certificate;

      if ContainsText(FSigner.SigningCertificate.KeyAlgorithm, 'id-dsa') then
      begin
        MessageDlg('The certificate was found to contain a DSA key. The hash algorithm has been switched to SHA1.', mtInformation, [mbOk], 0);
        FSigner.NewSignature.HashAlgorithm := 'SHA1';
      end;
    except
      on E: Exception do
        MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
    end;
  finally
    FreeAndNil(CertificateManager);
  end;

  FSigner.NewSignature.SignatureType := pstPAdES;

  case cbLevel.ItemIndex of
    0 : FSigner.NewSignature.Level := paslGeneric;
    1 : FSigner.NewSignature.Level := paslBaselineB;
    2 : FSigner.NewSignature.Level := paslBaselineT;
    3 : FSigner.NewSignature.Level := paslBaselineLT;
    4 : FSigner.NewSignature.Level := paslBaselineLTA;
    5 : FSigner.NewSignature.Level := paslBES;
    6 : FSigner.NewSignature.Level := paslEPES;
    7 : FSigner.NewSignature.Level := paslLTV;
  else
    FSigner.NewSignature.Level := paslBaselineB;
  end;

  FSigner.Widget.Invisible := not cbVisible.Checked;

  FSigner.IgnoreChainValidationErrors := true;

  FSigner.ExternalCrypto.Mode := TsbxExternalCryptoModes.ecmGeneric;

  try
    FSigner.SignExternal();

    MessageDlg('PDF file successfully signed', mtInformation, [mbOk], 0);
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormPdfsignerexternal.FormCreate(Sender: TObject);
begin
  FSigner := TsbxPDFSigner.Create(nil);
  FSigner.OnExternalSign := HandleExternalSign;
end;

procedure TFormPdfsignerexternal.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSigner);
end;

procedure TFormPdfsignerexternal.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edInputFile.Text;
  dlgOpen.Filter := 'PDF files (*.pdf)|*.pdf|All files (*.*)|*.*';
  if dlgOpen.Execute then
    edInputFile.Text := dlgOpen.FileName;
end;

procedure TFormPdfsignerexternal.sbOutputFileClick(Sender: TObject);
begin
  dlgSave.InitialDir := ExtractFilePath(Application.ExeName);
  dlgSave.Filter := 'PDF files (*.pdf)|*.pdf|All files (*.*)|*.*';
  if dlgSave.Execute then
    edOutputFile.Text := dlgSave.FileName;
end;

procedure TFormPdfsignerexternal.sbSignCertFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edSigningCertificate.Text;
  dlgOpen.Filter := 'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX';
  if dlgOpen.Execute then
    edSigningCertificate.Text := dlgOpen.FileName;
end;

procedure TFormPdfsignerexternal.sbSignKeyFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edSigningKey.Text;
  dlgOpen.Filter := 'PEM-encoded key (*.pem)|*.PEM|All files (*.*)|*.*';
  if dlgOpen.Execute then
    edSigningKey.Text := dlgOpen.FileName;
end;

procedure TFormPdfsignerexternal.HandleExternalSign(Sender: TObject;
  const OperationId: String; const HashAlgorithm: String; const Pars: String;
  const Data: String; var SignedData: String);
var
  KeyManager: TsbxCryptoKeyManager;
  Crypto: TsbxPublicKeyCrypto;
  InBuf, OutBuf: TBytes;
begin
  KeyManager := TsbxCryptoKeyManager.Create(nil);
  try
    try
      KeyManager.ImportFromFile(edSigningKey.Text, 3 {kffPEM}, '', '', Pars, 0 {ktAuto}, '');
    except
      on E: Exception do
      begin
        MessageDlg('Failed to load key', mtError, [mbOk], 0);
        exit;
      end;
    end;

    Crypto := TsbxPublicKeyCrypto.Create(nil);
    try
      Crypto.Key := KeyManager.Key;
      Crypto.HashAlgorithm := HashAlgorithm;
      Crypto.InputIsHash := true;
      Crypto.SchemeParams := Pars;

      InBuf := StringToBinary(Data);
      OutBuf := Crypto.Sign(InBuf, true);

      SignedData := BinaryToString(OutBuf);
    finally
      FreeAndNil(Crypto);
    end;
  finally
    FreeAndNil(KeyManager);
  end;
end;

end.
