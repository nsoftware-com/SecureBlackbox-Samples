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
unit soapsignerf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxCertificateManager, SBxSOAPSigner;

type
  TFormSoapsigner = class(TForm)
    lInputFile: TLabel;
    edInputFile: TEdit;
    sbBrowseInputFile: TSpeedButton;
    dlgOpenSOAP: TOpenDialog;
    lOutputFile: TLabel;
    edOutputFile: TEdit;
    sbBrowseOutputFile: TSpeedButton;
    dlgSaveSOAP: TSaveDialog;
    gbGeneralEnc: TGroupBox;
    lHashAlgorithm: TLabel;
    cmbHashAlgorithm: TComboBox;
    btnReferences: TButton;
    btnSign: TButton;
    lDemoInfo: TLabel;
    lSignatureType: TLabel;
    cmbSignatureType: TComboBox;
    cbSignBody: TCheckBox;
    gbKeyInfo: TGroupBox;
    Label5: TLabel;
    Label3: TLabel;
    sbSignCertFile: TSpeedButton;
    gbSigningCertificates: TGroupBox;
    lvSigningCertificates: TListView;
    btnRemove: TButton;
    btnAdd: TButton;
    edCertPassword: TEdit;
    edSigningCertificate: TEdit;
    cbIncludeKey: TCheckBox;
    cmbCertEmbed: TComboBox;
    edKeyName: TEdit;
    lCertEmbed: TLabel;
    lKeyName: TLabel;
    DlgOpen: TOpenDialog;
    procedure sbBrowseInputFileClick(Sender: TObject);
    procedure sbBrowseOutputFileClick(Sender: TObject);
    procedure btnReferencesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure cmbSignatureTypeChange(Sender: TObject);
    procedure sbSignCertFileClick(Sender: TObject);
  private
    { Private declarations }
    FSOAPSigner: TsbxSOAPSigner;

    function GetSignatureType: TsbxsoapSignatureTypes;
    function GetEmbedCertificateMethod: TsbxsoapsignerEmbedCertificateMethods;

    procedure UpdateCertificates;
  public
    { Public declarations }
  end;

var
  FormSoapsigner: TFormSoapsigner;

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

procedure TFormSoapsigner.btnReferencesClick(Sender: TObject);
var
  frmReferences: TFormReferences;
begin
  frmReferences := TFormReferences.Create(nil);
  try
    frmReferences.References := FSOAPSigner.References;
    frmReferences.ShowModal;
  finally
    FreeAndNil(frmReferences);
  end;
end;

procedure TFormSoapsigner.UpdateCertificates;
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
    for i := 0 to FSOAPSigner.SigningChain.Count - 1 do
    begin
      CertificateManager.Certificate := FSOAPSigner.SigningChain.Item[i];

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

procedure TFormSoapsigner.btnAddClick(Sender: TObject);
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

        FSOAPSigner.SigningChain.Add(CertificateManager.Certificate);

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

procedure TFormSoapsigner.btnRemoveClick(Sender: TObject);
begin
  if Assigned(lvSigningCertificates.Selected) then
  begin
    FSOAPSigner.SigningChain.RemoveAt(lvSigningCertificates.Selected.Index);
    UpdateCertificates;
  end;
end;

function TFormSoapsigner.GetSignatureType: TsbxsoapSignatureTypes;
begin
  case cmbSignatureType.ItemIndex of
    0 : Result := sstWSSSignature;
    1 : Result := sstSOAPSignature;
  else
    Result := sstUnknown;
  end;
end;

function TFormSoapsigner.GetEmbedCertificateMethod: TsbxsoapsignerEmbedCertificateMethods;
begin
  case cmbCertEmbed.ItemIndex of
    0: Result := cwecInSignature;
    1: Result := cwecInBinarySecurityToken;
    2: Result := cwecInSignedBinarySecurityToken;
    3: Result := cwecInBinarySecurityTokenAndSignature;
  else
    Result := cwecNone;
  end;
end;

procedure TFormSoapsigner.btnSignClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
begin
  FSOAPSigner.InputFile := edInputFile.Text;
  FSOAPSigner.OutputFile := edOutputFile.Text;

  CertificateManager := TsbxCertificateManager.Create(nil);
  try
    try
      CertificateManager.ImportFromFile(edSigningCertificate.Text, edCertPassword.Text);

      FSOAPSigner.SigningCertificate := CertificateManager.Certificate;
    except
      on E: Exception do
        MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
    end;
  finally
    FreeAndNil(CertificateManager);
  end;

  FSOAPSigner.NewSignature.SignatureType := GetSignatureType;

  FSOAPSigner.NewSignature.HashAlgorithm := cmbHashAlgorithm.Text;
  FSOAPSigner.EmbedCertificateMethod := GetEmbedCertificateMethod;

  FSOAPSigner.Config('KeyName=' + edKeyName.Text);
  if cbIncludeKey.Checked then
    FSOAPSigner.Config('IncludeKey=true')
  else
    FSOAPSigner.Config('IncludeKey=false');

  // Enable automatic signature formatting 
  FSOAPSigner.Config('XMLFormatting=auto');

  try
    if cbSignBody.Checked then
      FSOAPSigner.AddBodyReference('', true);

    FSOAPSigner.Sign();

    MessageDlg('SOAP message successfully signed', mtInformation, [mbOk], 0);
    Close;
  except
    on E: Exception do
      MessageDlg(E.Message, mtError, [mbOk], 0);
  end;
end;

procedure TFormSoapsigner.cmbSignatureTypeChange(Sender: TObject);
begin
  cmbCertEmbed.Visible := (cmbSignatureType.ItemIndex = 0);
  lCertEmbed.Visible := cmbCertEmbed.Visible;
  cbIncludeKey.Visible := (cmbSignatureType.ItemIndex = 1);
end;

procedure TFormSoapsigner.FormCreate(Sender: TObject);
begin
  FSOAPSigner := TsbxSOAPSigner.Create(nil);

  cmbSignatureType.ItemIndex := 0;
  cmbHashAlgorithm.ItemIndex := 0;
  cmbCertEmbed.ItemIndex := 1;
end;

procedure TFormSoapsigner.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSOAPSigner);
end;

procedure TFormSoapsigner.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpenSOAP.InitialDir := ExtractFilePath(Application.ExeName);
  dlgOpenSOAP.DefaultExt := 'xml';
  dlgOpenSOAP.Filter := 'XML and SOAP files (*.xml; *.soap)|*.xml; *.soap|All files|*.*';
  dlgOpenSOAP.FileName := edInputFile.Text;
  if dlgOpenSOAP.Execute then
    edInputFile.Text := dlgOpenSOAP.FileName;
end;

procedure TFormSoapsigner.sbBrowseOutputFileClick(Sender: TObject);
begin
  dlgSaveSOAP.InitialDir := ExtractFilePath(Application.ExeName);
  dlgSaveSOAP.DefaultExt := 'xml';
  dlgSaveSOAP.Filter := 'XML and SOAP files (*.xml; *.soap)|*.xml; *.soap|All files|*.*';
  if dlgSaveSOAP.Execute then
    edOutputFile.Text := dlgSaveSOAP.FileName;
end;

procedure TFormSoapsigner.sbSignCertFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edSigningCertificate.Text;
  dlgOpen.Filter := 'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX';
  if dlgOpen.Execute then
    edSigningCertificate.Text := dlgOpen.FileName;
end;

end.


