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
unit simplepdfsignerf;

interface

uses
  SysUtils, Variants, Classes, Graphics, StrUtils,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxCertificateManager, SBxCertificateStorage, SBxPDFSigner;

type
  TFormSimplepdfsigner = class(TForm)
    Label10: TLabel;
    lbInputFile: TLabel;
    sbBrowseInputFile: TSpeedButton;
    Label1: TLabel;
    sbOutputFile: TSpeedButton;
    edInputFile: TEdit;
    edOutputFile: TEdit;
    GroupBox6: TGroupBox;
    Label3: TLabel;
    sbCertFile: TSpeedButton;
    Label5: TLabel;
    edCertFile: TEdit;
    edCertPassword: TEdit;
    btnSign: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    GroupBox3: TGroupBox;
    lAuthorName: TLabel;
    editAuthorName: TEdit;
    lReason: TLabel;
    cbReason: TComboBox;
    editTSPServer: TEdit;
    cbRequestTimestamp: TCheckBox;
    rbCertFile: TRadioButton;
    rbPKCS11: TRadioButton;
    rbWin32: TRadioButton;
    edPKCS11File: TEdit;
    sbPKCS11File: TSpeedButton;
    Label2: TLabel;
    edPKCS11PIN: TEdit;
    edWin32Store: TEdit;
    procedure sbBrowseInputFileClick(Sender: TObject);
    procedure sbOutputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure sbCertFileClick(Sender: TObject);
    procedure sbPKCS11FileClick(Sender: TObject);
  private
    { Private declarations }
    FSigner: TsbxPDFSigner;
  public
    { Public declarations }
  end;

var
  FormSimplepdfsigner: TFormSimplepdfsigner;

implementation

{$R *.dfm}

function BinaryToString(const Buffer : TBytes): string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to Length(Buffer) - 1 do
    Result := Result + IntToHex(Buffer[i], 1);
end;

procedure TFormSimplepdfsigner.btnSignClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
  CertificateStorage: TsbxCertificateStorage;
begin
  CertificateStorage := TsbxCertificateStorage.Create(nil);
  try
    FSigner.InputFile := edInputFile.Text;
    FSigner.OutputFile := edOutputFile.Text;

    if rbCertFile.Checked then
    begin
      CertificateManager := TsbxCertificateManager.Create(nil);
      try
        try
          CertificateManager.ImportFromFile(edCertFile.Text, edCertPassword.Text);

          FSigner.SigningCertificate := CertificateManager.Certificate;
        except
          on E: Exception do
            MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
        end;
      finally
        FreeAndNil(CertificateManager);
      end;
    end
    else
    begin
      try
        if rbPKCS11.Checked then
          CertificateStorage.Open('pkcs11://user:' + edPKCS11PIN.Text + '@/' + edPKCS11File.Text)
        else
          CertificateStorage.Open('system://?store=' + edWin32Store.Text);

        FSigner.SigningCertificate := CertificateStorage.Certificates.Item[0];
      except
        on E: Exception do
          MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
      end;
    end;

    if ContainsText(FSigner.SigningCertificate.KeyAlgorithm, 'id-dsa') then
    begin
      MessageDlg('The certificate was found to contain a DSA key. The hash algorithm has been switched to SHA1.', mtInformation, [mbOk], 0);
      FSigner.NewSignature.HashAlgorithm := 'SHA1';
    end;

    FSigner.NewSignature.SignatureType := pstPAdES;
    FSigner.NewSignature.Level := paslBES;
    FSigner.Widget.Invisible := false;
    FSigner.IgnoreChainValidationErrors := true;

    FSigner.NewSignature.AuthorName := {$ifndef UNICODE}MultiByteToUTF8{$endif}(editAuthorName.Text);
    if CompareStr(cbReason.Text, '<none>') <> 0 then
      FSigner.NewSignature.Reason := {$ifndef UNICODE}MultiByteToUTF8{$endif}(cbReason.Text)
    else
      FSigner.NewSignature.Reason := '';

    if cbRequestTimestamp.Checked then
      FSigner.TimestampServer := editTSPServer.Text
    else
      FSigner.TimestampServer := '';

    try
      FSigner.Sign();

      MessageDlg('PDF file successfully signed', mtInformation, [mbOk], 0);
    except
      on E: Exception do
        MessageDlg(e.message, mtError, [mbOk], 0);
    end;
  finally
    FreeAndNil(CertificateStorage);
  end;
end;

procedure TFormSimplepdfsigner.FormCreate(Sender: TObject);
begin
  FSigner := TsbxPDFSigner.Create(nil);
end;

procedure TFormSimplepdfsigner.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSigner);
end;

procedure TFormSimplepdfsigner.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edInputFile.Text;
  dlgOpen.Filter := 'PDF files (*.pdf)|*.pdf|All files (*.*)|*.*';
  if dlgOpen.Execute then
    edInputFile.Text := dlgOpen.FileName;
end;

procedure TFormSimplepdfsigner.sbOutputFileClick(Sender: TObject);
begin
  dlgSave.InitialDir := ExtractFilePath(Application.ExeName);
  dlgSave.Filter := 'PDF files (*.pdf)|*.pdf|All files (*.*)|*.*';
  if dlgSave.Execute then
    edOutputFile.Text := dlgSave.FileName;
end;

procedure TFormSimplepdfsigner.sbPKCS11FileClick(Sender: TObject);
begin
  dlgOpen.FileName := edPKCS11File.Text;
  dlgOpen.Filter := 'PKCS #11 Library (*.dll)|*.dll';
  if dlgOpen.Execute then
    edPKCS11File.Text := dlgOpen.FileName;
end;

procedure TFormSimplepdfsigner.sbCertFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edCertFile.Text;
  dlgOpen.Filter := 'Certificates (*.pem, *.cer, *.crt, *.der, *.pfx, *.p12, *.pkcs12)|*.pem;*.cer;*.crt;*.der;*.pfx;*.p12;*.pkcs12|PEM-encoded certificates (*.pem)|*.pem|' +
    'DER-encoded certificates (*.cer, *.crt, *.der)|*.cer;*.crt;*.der|PKCS#12 encoded certificates (*.pfx, *.p12, *.pkcs12)|*.pfx;*.p12;*.pkcs12|All files (*.*)|*.*';
  if dlgOpen.Execute then
    edCertFile.Text := dlgOpen.FileName;
end;

end.









