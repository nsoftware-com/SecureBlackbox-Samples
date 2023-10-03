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
    GroupBox1: TGroupBox;
    cmbVersion: TComboBox;
    lbVersion: TLabel;
    cbTimestamp: TCheckBox;
    edTimestampServer: TEdit;
    gbGeneralEnc: TGroupBox;
    lbHashAlgorithm: TLabel;
    lbCanonMethod: TLabel;
    cmbHashAlgorithm: TComboBox;
    cmbCanonMethod: TComboBox;
    cbDetached: TCheckBox;
    cmbForm: TComboBox;
    Label2: TLabel;
    Label10: TLabel;
    gbKeyInfo: TGroupBox;
    lbKeyName: TLabel;
    Label5: TLabel;
    Label3: TLabel;
    sbSignCertFile: TSpeedButton;
    edKeyName: TEdit;
    cbIncludeKey: TCheckBox;
    gbSigningCertificates: TGroupBox;
    lvSigningCertificates: TListView;
    btnRemove: TButton;
    btnAdd: TButton;
    edCertPassword: TEdit;
    edSigningCertificate: TEdit;
    FSigner: TsbxXAdESSigner;
    cbEnableXAdES: TCheckBox;
    lbSignatureLocation: TLabel;
    edSignatureLocation: TEdit;
    procedure sbBrowseXMLFileClick(Sender: TObject);
    procedure sbOutputFileClick(Sender: TObject);
    procedure btnReferencesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure sbSignCertFileClick(Sender: TObject);
  private
    { Private declarations }

    procedure UpdateCertificates;
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

procedure TFormXadessigner.UpdateCertificates;
var
  Item: TListItem;
  i: Integer;
  s: string;
  CertificateManager: TsbxCertificateManager;
begin
  lvSigningCertificates.Items.BeginUpdate;
  lvSigningCertificates.Items.Clear;

  CertificateManager := TsbxCertificateManager.Create(nil);
  try
    for i := 0 to FSigner.SigningChain.Count - 1 do
    begin
      CertificateManager.Certificate := FSigner.SigningChain.Item[i];

      s := CertificateManager.Certificate.Issuer;
      if s = '' then
        s := '<unknown>';

      Item := lvSigningCertificates.Items.Add;
      Item.Caption := BinaryToString(CertificateManager.Certificate.SerialNumber);
      Item.SubItems.Add(s);
    end;
  finally
    FreeAndNil(CertificateManager);
  end;

  lvSigningCertificates.Items.EndUpdate;
end;

procedure TFormXadessigner.btnAddClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
begin
  DlgOpen.Title := 'Select certificate file';
  DlgOpen.Filter := 'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX';
  if DlgOpen.Execute then
  begin
    CertificateManager := TsbxCertificateManager.Create(nil);
    try
      try
        CertificateManager.ImportFromFile(DlgOpen.Filename, InputBox('Please enter passphrase:', '',''));

        FSigner.SigningChain.Add(CertificateManager.Certificate);

        UpdateCertificates;
      except
        on E: Exception do
          MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
      end;
    finally
      FreeAndNil(CertificateManager);
    end;
  end;
end;

procedure TFormXadessigner.btnRemoveClick(Sender: TObject);
begin
  if Assigned(lvSigningCertificates.Selected) then
  begin
    FSigner.SigningChain.RemoveAt(lvSigningCertificates.Selected.Index);
    UpdateCertificates;
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

  FSigner.NewSignature.XMLElement := edSignatureLocation.Text;

  // XAdES options
  FSigner.NewSignature.XAdES := cbEnableXAdES.Checked;
  if cbEnableXAdES.Checked then
  begin
    case (cmbVersion.ItemIndex) of
      0:
        FSigner.NewSignature.XAdESVersion := xav111;
      1:
        FSigner.NewSignature.XAdESVersion := xav122;
      3:
        FSigner.NewSignature.XAdESVersion := xav141;
      else
        FSigner.NewSignature.XAdESVersion := xav132;
    end;

    case (cmbForm.ItemIndex) of
      0:
        FSigner.NewSignature.XAdESForm := xafBasic;
      1:
        FSigner.NewSignature.XAdESForm := xafBES;
      2:
        FSigner.NewSignature.XAdESForm := xafEPES;
      3:
        FSigner.NewSignature.XAdESForm := xafT;
      4:
        FSigner.NewSignature.XAdESForm := xafC;
      5:
        FSigner.NewSignature.XAdESForm := xafX;
      6:
        FSigner.NewSignature.XAdESForm := xafXL;
      7:
        FSigner.NewSignature.XAdESForm := xafA;
      8:
        FSigner.NewSignature.XAdESForm := xafExtendedBES;
      9:
        FSigner.NewSignature.XAdESForm := xafExtendedEPES;
      10:
        FSigner.NewSignature.XAdESForm := xafExtendedT;
      11:
        FSigner.NewSignature.XAdESForm := xafExtendedC;
      12:
        FSigner.NewSignature.XAdESForm := xafExtendedX;
      13:
        FSigner.NewSignature.XAdESForm := xafExtendedXLong;
      14:
        FSigner.NewSignature.XAdESForm := xafExtendedXL;
      15:
        FSigner.NewSignature.XAdESForm := xafExtendedA;
      else
        FSigner.NewSignature.XAdESForm := xafBES;
    end;

    if cbTimestamp.Checked then
      FSigner.TimestampServer := edTimestampServer.Text;
  end;

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
  cmbCanonMethod.ItemIndex := 0;
  cmbHashAlgorithm.ItemIndex := 0;
  cmbVersion.ItemIndex := 2;
  cmbForm.ItemIndex := 1;
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
  dlgOpen.Filter := 'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX';
  if dlgOpen.Execute then
    edSigningCertificate.Text := dlgOpen.FileName;
end;

end.

