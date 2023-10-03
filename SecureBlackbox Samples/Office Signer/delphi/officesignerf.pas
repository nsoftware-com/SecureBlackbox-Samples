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
unit officesignerf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxCertificateManager, SBxOfficeSigner;

type
  TFormOfficesigner = class(TForm)
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
    edSigningCertificate: TEdit;
    edCertPassword: TEdit;
    GroupBox5: TGroupBox;
    lvSigningCertificates: TListView;
    btnRemoveCert: TButton;
    btnAddCert: TButton;
    btnSign: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    cbSignatureType: TComboBox;
    Label4: TLabel;
    Label2: TLabel;
    cbHashAlgorithm: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox3: TGroupBox;
    Label7: TLabel;
    Label11: TLabel;
    edSigText: TEdit;
    edSigComments: TEdit;
    cbIncludeSigInfo: TCheckBox;
    cbSignDocument: TCheckBox;
    cbSignSignatureOrigin: TCheckBox;
    cbSignCoreProperties: TCheckBox;
    procedure sbBrowseInputFileClick(Sender: TObject);
    procedure sbOutputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure sbSignCertFileClick(Sender: TObject);
    procedure btnAddCertClick(Sender: TObject);
    procedure btnRemoveCertClick(Sender: TObject);
  private
    { Private declarations }
    FSigner: TsbxOfficeSigner;

    procedure UpdateSigningCertificates;
  public
    { Public declarations }
  end;

var
  FormOfficesigner: TFormOfficesigner;

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

procedure TFormOfficesigner.UpdateSigningCertificates;
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

procedure TFormOfficesigner.btnAddCertClick(Sender: TObject);
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

        UpdateSigningCertificates;
      except
        on E: Exception do
          MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
      end;
    finally
      FreeAndNil(CertificateManager);
    end;
  end;
end;

procedure TFormOfficesigner.btnRemoveCertClick(Sender: TObject);
begin
  if Assigned(lvSigningCertificates.Selected) then
  begin
    FSigner.SigningChain.RemoveAt(lvSigningCertificates.Selected.Index);
    UpdateSigningCertificates;
  end;
end;

procedure TFormOfficesigner.btnSignClick(Sender: TObject);
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
    except
      on E: Exception do
        MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
    end;
  finally
    FreeAndNil(CertificateManager);
  end;

  case cbSignatureType.ItemIndex of
    0: FSigner.NewSignature.SignatureType := ostDefault;
    1: FSigner.NewSignature.SignatureType := ostBinaryCryptoAPI;
    2: FSigner.NewSignature.SignatureType := ostBinaryXML;
    3: FSigner.NewSignature.SignatureType := ostOpenXML;
    4: FSigner.NewSignature.SignatureType := ostOpenXPS;
    5: FSigner.NewSignature.SignatureType := ostOpenDocument;
  end;

  FSigner.NewSignature.HashAlgorithm := cbHashAlgorithm.Text;

  FSigner.NewSignature.DocumentSigned := cbSignDocument.Checked;
  FSigner.NewSignature.CorePropertiesSigned := cbSignCoreProperties.Checked;
  FSigner.NewSignature.SignatureOriginSigned := cbSignSignatureOrigin.Checked;

  if cbIncludeSigInfo.Checked then
    FSigner.Config('SignatureInfoIncluded=true')
  else
    FSigner.Config('SignatureInfoIncluded=false');

  FSigner.Config('SignatureInfoText=' + edSigText.Text);
  FSigner.Config('SignatureInfoComments=' + edSigComments.Text);

  try
    FSigner.Sign();

    MessageDlg('Office file successfully signed', mtInformation, [mbOk], 0);
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormOfficesigner.FormCreate(Sender: TObject);
begin
  FSigner := TsbxOfficeSigner.Create(nil);
end;

procedure TFormOfficesigner.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSigner);
end;

procedure TFormOfficesigner.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edInputFile.Text;
  dlgOpen.Filter :=       
      'All Documents|*.doc; *.dot; *.docx; *.docm; *.dotx; *.dotm; *.xl' +
      's; *.xlt; *.xlm; *.xlsx; *.xlsm; *.xlsb; *.xltx; *.xltm; *.ppt; ' +
      '*.ppm; *.pot; *.pps; *.pptx; *.pptm; *.potm; *.ppsx; *.ppsm; *.x' +
      'ps; *.odt; *.ott; *.odg; *.otg; *.odp; *.otp; *.ods; *.ots; *.od' +
      'c; *.otc; *.odi; *.oti; *.odf; *.otf; *.odm; *.oth; *.odb|All fi' +
      'les (*.*)|*.*|Word Document (*.doc; *.dot; *.docx; *.docm; *.dot' +
      'x; *.dotm)|*.doc; *.dot; *.docx; *.docm; *.dotx; *.dotm|Excel (*' +
      '.xls; *.xlt; *.xlm; *.xlsx; *.xlsm; *.xlsb; *.xltx; *.xltm)|*.xl' +
      's; *.xlt; *.xlm; *.xlsx; *.xlsm; *.xlsb; *.xltx; *.xltm|PowerPoi' +
      'nt (*.ppt; *.ppm; *.pot; *.pps; *.pptx; *.pptm; *.potm; *.ppsx; ' +
      '*.ppsm)|*.ppt; *.ppm; *.pot; *.pps; *.pptx; *.pptm; *.potm; *.pp' +
      'sx; *.ppsm|Open XPS (*.xps)|*.xps|OpenDocument Format|*.odt; *.o' +
      'tt; *.odg; *.otg; *.odp; *.otp; *.ods; *.ots; *.odc; *.otc; *.od' +
      'i; *.oti; *.odf; *.otf; *.odm; *.oth; *.odb';

  if dlgOpen.Execute then
    edInputFile.Text := dlgOpen.FileName;
end;

procedure TFormOfficesigner.sbOutputFileClick(Sender: TObject);
begin
  dlgSave.InitialDir := ExtractFilePath(Application.ExeName);
  if dlgSave.Execute then
    edOutputFile.Text := dlgSave.FileName;
end;

procedure TFormOfficesigner.sbSignCertFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edSigningCertificate.Text;
  dlgOpen.Filter := 'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX';
  if dlgOpen.Execute then
    edSigningCertificate.Text := dlgOpen.FileName;
end;

end.







