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
unit xmlsignerf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls, ExtCtrls,
  SBxTypes, SBxCore, SBxCertificateManager, SBxXMLSigner;

type
  TFormXmlsigner = class(TForm)
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
    gbGeneralEnc: TGroupBox;
    lbHashAlgorithm: TLabel;
    lbCanonMethod: TLabel;
    cmbHashAlgorithm: TComboBox;
    cmbCanonMethod: TComboBox;
    cbDetached: TCheckBox;
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
    FSigner: TsbxXMLSigner;
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
  FormXmlsigner: TFormXmlsigner;

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

procedure TFormXmlsigner.btnReferencesClick(Sender: TObject);
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

procedure TFormXmlsigner.UpdateCertificates;
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

procedure TFormXmlsigner.btnAddClick(Sender: TObject);
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

procedure TFormXmlsigner.btnRemoveClick(Sender: TObject);
begin
  if Assigned(lvSigningCertificates.Selected) then
  begin
    FSigner.SigningChain.RemoveAt(lvSigningCertificates.Selected.Index);
    UpdateCertificates;
  end;
end;

procedure TFormXmlsigner.btnSignClick(Sender: TObject);
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
    FSigner.SignatureType := cxstDetached;
  end
  else
  begin
    FSigner.InputFile := edXMLFile.Text;
    FSigner.SignatureType := cxstEnveloped;
  end;

  case (cmbCanonMethod.ItemIndex) of
    0:
      FSigner.CanonicalizationMethod := cxcmCanon;
    1:
      FSigner.CanonicalizationMethod := cxcmCanonComment;
    2:
      FSigner.CanonicalizationMethod := cxcmCanon_v1_1;
    3:
      FSigner.CanonicalizationMethod := cxcmCanonComment_v1_1;
    4:
      FSigner.CanonicalizationMethod := cxcmExclCanon;
    5:
      FSigner.CanonicalizationMethod := cxcmExclCanonComment;
    6:
      FSigner.CanonicalizationMethod := cxcmMinCanon;
    else
      FSigner.CanonicalizationMethod := cxcmCanon;
  end;

  FSigner.HashAlgorithm := cmbHashAlgorithm.Text;

  FSigner.Config('KeyName=' + edKeyName.Text);
  if cbIncludeKey.Checked then
    FSigner.Config('IncludeKey=true')
  else
    FSigner.Config('IncludeKey=false');

  // Enable automatic signature formatting 
  FSigner.Config('XMLFormatting=auto');

  FSigner.XMLElement := edSignatureLocation.Text;

  try
    FSigner.Sign();

    MessageDlg('XML file successfully signed', mtInformation, [mbOk], 0);
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormXmlsigner.FormCreate(Sender: TObject);
begin
  cmbCanonMethod.ItemIndex := 0;
  cmbHashAlgorithm.ItemIndex := 0;
end;

procedure TFormXmlsigner.sbBrowseXMLFileClick(Sender: TObject);
begin
  dlgOpenXML.InitialDir := ExtractFilePath(Application.ExeName);
  dlgOpenXML.FileName := edXMLFile.Text;
  if dlgOpenXML.Execute then
    edXMLFile.Text := dlgOpenXML.FileName;
end;

procedure TFormXmlsigner.sbOutputFileClick(Sender: TObject);
begin
  dlgSaveXML.InitialDir := ExtractFilePath(Application.ExeName);
  if dlgSaveXML.Execute then
    edOutputFile.Text := dlgSaveXML.FileName;
end;

procedure TFormXmlsigner.sbSignCertFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edSigningCertificate.Text;
  dlgOpen.Filter := 'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX';
  if dlgOpen.Execute then
    edSigningCertificate.Text := dlgOpen.FileName;
end;

end.

