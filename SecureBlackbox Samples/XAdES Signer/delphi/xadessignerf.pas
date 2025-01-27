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
unit xadessignerf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls, ExtCtrls,
  SBxTypes, SBxCore, SBxCertificateManager, SBxXAdESSigner;

type
  TFormXadessigner = class(TForm)
    lbXMLFile: TLabel;
    edXMLFile: TEdit;
    sbBrowseXMLFile: TSpeedButton;
    dlgOpenXML: TOpenDialog;
    Label1: TLabel;
    edOutputFile: TEdit;
    sbOutputFile: TSpeedButton;
    dlgSaveXML: TSaveDialog;
    btnReferences: TButton;
    btnSign: TButton;
    DlgOpen: TOpenDialog;
    Label10: TLabel;
    gbSigningOptions: TGroupBox;
    lbKeyName: TLabel;
    Label5: TLabel;
    Label3: TLabel;
    sbSignCertFile: TSpeedButton;
    edKeyName: TEdit;
    cbIncludeKey: TCheckBox;
    edCertPassword: TEdit;
    edSigningCertificate: TEdit;
    cmbVersion: TComboBox;
    lbVersion: TLabel;
    cmbLevel: TComboBox;
    Label2: TLabel;
    cbTimestamp: TCheckBox;
    edTimestampServer: TEdit;
    cbDetached: TCheckBox;
    lbCanonMethod: TLabel;
    cmbCanonMethod: TComboBox;
    lbHashAlgorithm: TLabel;
    cmbHashAlgorithm: TComboBox;
    lbSignatureLocation: TLabel;
    edSignatureNode: TEdit;
    procedure sbBrowseXMLFileClick(Sender: TObject);
    procedure sbOutputFileClick(Sender: TObject);
    procedure btnReferencesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure sbSignCertFileClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSigner: TsbxXAdESSigner;
  public
    { Public declarations }
  end;

var
  FormXadessigner: TFormXadessigner;

implementation

{$R *.dfm}

uses referencesf;

function BinaryToString(const Buffer : TBytes): string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to Length(Buffer) - 1 do
    Result := Result + IntToHex(Buffer[i], 1);
end;

procedure TFormXadessigner.btnReferencesClick(Sender: TObject);
var
  frmReferences: TFormReferences;
begin
  frmReferences := TFormReferences.Create(nil);
  try
    frmReferences.References := FSigner.References;
    frmReferences.ShowModal;
  finally
    FreeAndNil(frmReferences);
  end;
end;

procedure TFormXadessigner.btnSignClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
begin
  FSigner.OutputFile := edOutputFile.Text;

  CertificateManager := TsbxCertificateManager.Create(nil);
  try
    try
      CertificateManager.ImportFromFile(edSigningCertificate.Text, edCertPassword.Text);

      FSigner.SigningCertificate := CertificateManager.Certificate;
    except
      on E: Exception do
        MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
    end;
  finally
    FreeAndNil(CertificateManager);
  end;

  if cbDetached.Checked then
  begin
    FSigner.DataFile := edXMLFile.Text;
    FSigner.DataType := cxdtBinary;
    FSigner.DataURI := ExtractFileName(edXMLFile.Text);
    FSigner.NewSignature.SignatureType := cxstDetached;
  end
  else
  begin
    FSigner.InputFile := edXMLFile.Text;
    FSigner.NewSignature.SignatureType := cxstEnveloped;
  end;

  case (cmbCanonMethod.ItemIndex) of
    0:
      FSigner.NewSignature.CanonicalizationMethod := cxcmCanon;
    1:
      FSigner.NewSignature.CanonicalizationMethod := cxcmCanonComment;
    2:
      FSigner.NewSignature.CanonicalizationMethod := cxcmCanon_v1_1;
    3:
      FSigner.NewSignature.CanonicalizationMethod := cxcmCanonComment_v1_1;
    4:
      FSigner.NewSignature.CanonicalizationMethod := cxcmExclCanon;
    5:
      FSigner.NewSignature.CanonicalizationMethod := cxcmExclCanonComment;
    6:
      FSigner.NewSignature.CanonicalizationMethod := cxcmMinCanon;
    else
      FSigner.NewSignature.CanonicalizationMethod := cxcmCanon;
  end;

  FSigner.NewSignature.HashAlgorithm := cmbHashAlgorithm.Text;

  FSigner.Config('KeyName=' + edKeyName.Text);
  if cbIncludeKey.Checked then
    FSigner.Config('IncludeKey=true')
  else
    FSigner.Config('IncludeKey=false');

  // Enable automatic signature formatting
  FSigner.Config('XMLFormatting=auto');

  FSigner.NewSignature.XMLElement := edSignatureNode.Text;

  // XAdES options
  case (cmbVersion.ItemIndex) of
    0: FSigner.NewSignature.XAdESVersion := xav111;
    1: FSigner.NewSignature.XAdESVersion := xav122;
    3: FSigner.NewSignature.XAdESVersion := xav141;
    else
      FSigner.NewSignature.XAdESVersion := xav132;
  end;

  case (cmbLevel.ItemIndex) of
    0: FSigner.NewSignature.Level := aslBaselineB;
    1: FSigner.NewSignature.Level := aslBaselineT;
    2: FSigner.NewSignature.Level := aslBaselineLT;
    3: FSigner.NewSignature.Level := aslBaselineLTA;
    4: FSigner.NewSignature.Level := aslBES;
    5: FSigner.NewSignature.Level := aslEPES;
    6: FSigner.NewSignature.Level := aslT;
    7: FSigner.NewSignature.Level := aslC;
    8: FSigner.NewSignature.Level := aslX;
    9: FSigner.NewSignature.Level := aslXL;
    10: FSigner.NewSignature.Level := aslA;
    11: FSigner.NewSignature.Level := aslExtendedBES;
    12: FSigner.NewSignature.Level := aslExtendedEPES;
    13: FSigner.NewSignature.Level := aslExtendedT;
    14: FSigner.NewSignature.Level := aslExtendedC;
    15: FSigner.NewSignature.Level := aslExtendedX;
    16: FSigner.NewSignature.Level := aslExtendedXLong;
    17: FSigner.NewSignature.Level := aslExtendedXL;
    18: FSigner.NewSignature.Level := aslExtendedA;
    19: FSigner.NewSignature.Level := aslGeneric;
    else
      FSigner.NewSignature.Level := aslBES;
  end;

  if cbTimestamp.Checked then
    FSigner.TimestampServer := edTimestampServer.Text;

  try
    FSigner.Sign();

    MessageDlg('XML file successfully signed', mtInformation, [mbOk], 0);
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormXadessigner.FormCreate(Sender: TObject);
begin
  FSigner := TsbxXAdESSigner.Create(nil);

  cmbCanonMethod.ItemIndex := 0;
  cmbHashAlgorithm.ItemIndex := 2;
  cmbVersion.ItemIndex := 2;
  cmbLevel.ItemIndex := 4;
end;

procedure TFormXadessigner.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSigner);
end;

procedure TFormXadessigner.sbBrowseXMLFileClick(Sender: TObject);
begin
  dlgOpenXML.InitialDir := ExtractFilePath(Application.ExeName);
  dlgOpenXML.FileName := edXMLFile.Text;
  if dlgOpenXML.Execute then
    edXMLFile.Text := dlgOpenXML.FileName;
end;

procedure TFormXadessigner.sbOutputFileClick(Sender: TObject);
begin
  dlgSaveXML.InitialDir := ExtractFilePath(Application.ExeName);
  if dlgSaveXML.Execute then
    edOutputFile.Text := dlgSaveXML.FileName;
end;

procedure TFormXadessigner.sbSignCertFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edSigningCertificate.Text;
  dlgOpen.Filter := 'Certificates (*.pem, *.cer, *.crt, *.der, *.pfx, *.p12, *.pkcs12)|*.pem;*.cer;*.crt;*.der;*.pfx;*.p12;*.pkcs12|PEM-encoded certificates (*.pem)|*.pem|' +
    'DER-encoded certificates (*.cer, *.crt, *.der)|*.cer;*.crt;*.der|PKCS#12 encoded certificates (*.pfx, *.p12, *.pkcs12)|*.pfx;*.p12;*.pkcs12|All files (*.*)|*.*';
  if dlgOpen.Execute then
    edSigningCertificate.Text := dlgOpen.FileName;
end;

end.
