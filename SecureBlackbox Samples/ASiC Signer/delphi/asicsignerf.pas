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
unit asicsignerf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxCertificateManager, SBxASiCSigner;

type
  TFormAsicsigner = class(TForm)
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    btnSign: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    lvKnownCertificates: TListView;
    btnRemoveKnown: TButton;
    btnAddKnown: TButton;
    GroupBox4: TGroupBox;
    lvTrustedCertificates: TListView;
    btnRemoveTrusted: TButton;
    btnAddTrusted: TButton;
    cbLevel: TComboBox;
    Label4: TLabel;
    Label1: TLabel;
    sbOutputFile: TSpeedButton;
    Label8: TLabel;
    lbInputFileName: TLabel;
    edOutputFile: TEdit;
    lbSourceFiles: TListBox;
    bbClear: TButton;
    btnBrowseInputFile: TButton;
    cbSignatureType: TComboBox;
    GroupBox6: TGroupBox;
    Label2: TLabel;
    sbSignCertFile: TSpeedButton;
    Label3: TLabel;
    edSigningCertificate: TEdit;
    edCertPassword: TEdit;
    Label10: TLabel;
    GroupBox5: TGroupBox;
    lvSigningCertificates: TListView;
    btnRemoveCert: TButton;
    btnAddCert: TButton;
    GroupBox3: TGroupBox;
    GroupBox7: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    cbHashAlgorithm: TComboBox;
    edIdentifier: TEdit;
    edHashValue: TEdit;
    cbRequestTimestamp: TCheckBox;
    editTSPServer: TEdit;
    cbExtended: TCheckBox;
    procedure sbOutputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure btnAddKnownClick(Sender: TObject);
    procedure btnAddTrustedClick(Sender: TObject);
    procedure btnRemoveKnownClick(Sender: TObject);
    procedure btnRemoveTrustedClick(Sender: TObject);
    procedure sbSignCertFileClick(Sender: TObject);
    procedure bbClearClick(Sender: TObject);
    procedure btnAddCertClick(Sender: TObject);
    procedure btnBrowseInputFileClick(Sender: TObject);
    procedure btnRemoveCertClick(Sender: TObject);
  private
    { Private declarations }
    FSigner: TsbxASiCSigner;

    procedure UpdateSigningCertificates;
    procedure UpdateKnownCertificates;
    procedure UpdateTrustedCertificates;
  public
    { Public declarations }
  end;

var
  FormAsicsigner: TFormAsicsigner;

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

procedure TFormAsicsigner.UpdateSigningCertificates;
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

procedure TFormAsicsigner.UpdateKnownCertificates;
var
  Item: TListItem;
  i: Integer;
  s: string;
  CertificateManager: TsbxCertificateManager;
begin
  lvKnownCertificates.Items.BeginUpdate;
  lvKnownCertificates.Items.Clear;

  CertificateManager := TsbxCertificateManager.Create(nil);
  try
    for i := 0 to FSigner.KnownCertificates.Count - 1 do
    begin
      CertificateManager.Certificate := FSigner.KnownCertificates.Item[i];

      s := CertificateManager.Certificate.Issuer;
      if s = '' then
        s := '<unknown>';

      Item := lvKnownCertificates.Items.Add;
      Item.Caption := BinaryToString(CertificateManager.Certificate.SerialNumber);
      Item.SubItems.Add(s);
    end;
  finally
    FreeAndNil(CertificateManager);
  end;

  lvKnownCertificates.Items.EndUpdate;
end;

procedure TFormAsicsigner.UpdateTrustedCertificates;
var
  Item: TListItem;
  i: Integer;
  s: string;
  CertificateManager: TsbxCertificateManager;
begin
  lvTrustedCertificates.Items.BeginUpdate;
  lvTrustedCertificates.Items.Clear;

  CertificateManager := TsbxCertificateManager.Create(nil);
  try
    for i := 0 to FSigner.TrustedCertificates.Count - 1 do
    begin
      CertificateManager.Certificate := FSigner.TrustedCertificates.Item[i];

      s := CertificateManager.Certificate.Issuer;
      if s = '' then
        s := '<unknown>';

      Item := lvTrustedCertificates.Items.Add;
      Item.Caption := BinaryToString(CertificateManager.Certificate.SerialNumber);
      Item.SubItems.Add(s);
    end;
  finally
    FreeAndNil(CertificateManager);
  end;

  lvTrustedCertificates.Items.EndUpdate;
end;

procedure TFormAsicsigner.bbClearClick(Sender: TObject);
begin
  lbSourceFiles.Items.Clear;
end;

procedure TFormAsicsigner.btnAddCertClick(Sender: TObject);
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

procedure TFormAsicsigner.btnAddKnownClick(Sender: TObject);
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

        FSigner.KnownCertificates.Add(CertificateManager.Certificate);

        UpdateKnownCertificates;
      except
        on E: Exception do
          MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
      end;
    finally
      FreeAndNil(CertificateManager);
    end;
  end;
end;

procedure TFormAsicsigner.btnAddTrustedClick(Sender: TObject);
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

        FSigner.TrustedCertificates.Add(CertificateManager.Certificate);

        UpdateTrustedCertificates;
      except
        on E: Exception do
          MessageDlg('Failed to load certificate', mtError, [mbOk], 0);
      end;
    finally
      FreeAndNil(CertificateManager);
    end;
  end;
end;

procedure TFormAsicsigner.btnBrowseInputFileClick(Sender: TObject);
begin
  dlgOpen.Filter := '';
  dlgOpen.Title := 'Please, select file';
  if dlgOpen.Execute then
    lbSourceFiles.Items.Add(dlgOpen.FileName);
end;

procedure TFormAsicsigner.btnRemoveCertClick(Sender: TObject);
begin
  if Assigned(lvSigningCertificates.Selected) then
  begin
    FSigner.SigningChain.RemoveAt(lvSigningCertificates.Selected.Index);
    UpdateSigningCertificates;
  end;
end;

procedure TFormAsicsigner.btnRemoveKnownClick(Sender: TObject);
begin
  if Assigned(lvKnownCertificates.Selected) then
  begin
    FSigner.KnownCertificates.RemoveAt(lvKnownCertificates.Selected.Index);
    UpdateKnownCertificates;
  end;
end;

procedure TFormAsicsigner.btnRemoveTrustedClick(Sender: TObject);
begin
  if Assigned(lvTrustedCertificates.Selected) then
  begin
    FSigner.TrustedCertificates.RemoveAt(lvTrustedCertificates.Selected.Index);
    UpdateTrustedCertificates;
  end;
end;

procedure TFormAsicsigner.btnSignClick(Sender: TObject);
var
  i: integer;
  CertificateManager: TsbxCertificateManager;
begin
  if lbSourceFiles.Items.Count > 0 then
  begin
    FSigner.SourceFiles := lbSourceFiles.Items[0];
    for i := 1 to lbSourceFiles.Items.Count - 1 do
      FSigner.SourceFiles := FSigner.SourceFiles + #13#10 + lbSourceFiles.Items[i];
  end
  else
    FSigner.SourceFiles := '';
  FSigner.OutputFile := edOutputFile.Text;

  FSigner.Extended := cbExtended.Checked;

//  case cbSignatureType.ItemIndex of
//    0: FSigner.SignatureType := castCAdES;
//    1: FSigner.SignatureType := castXAdES;
//  end;

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



  case cbLevel.ItemIndex of
    0: FSigner.NewSignature.Level := aslBES;
    1: FSigner.NewSignature.Level := aslEPES;
    2: FSigner.NewSignature.Level := aslT;
    3: FSigner.NewSignature.Level := aslC;
    4: FSigner.NewSignature.Level := aslXType1;
    5: FSigner.NewSignature.Level := aslXType2;
    6: FSigner.NewSignature.Level := aslXLType1;
    7: FSigner.NewSignature.Level := aslXLType2;
    8: FSigner.NewSignature.Level := aslBaselineB;
    9: FSigner.NewSignature.Level := aslBaselineT;
    10: FSigner.NewSignature.Level := aslBaselineLT;
    11: FSigner.NewSignature.Level := aslBaselineLTA;
    12: FSigner.NewSignature.Level := aslExtendedBES;
    13: FSigner.NewSignature.Level := aslExtendedEPES;
    14: FSigner.NewSignature.Level := aslExtendedT;
    15: FSigner.NewSignature.Level := aslExtendedC;
    16: FSigner.NewSignature.Level := aslExtendedXType1;
    17: FSigner.NewSignature.Level := aslExtendedXType2;
    18: FSigner.NewSignature.Level := aslExtendedXLType1;
    19: FSigner.NewSignature.Level := aslExtendedXLType2;
    20: FSigner.NewSignature.Level := aslA;
    21: FSigner.NewSignature.Level := aslExtendedA;
  end;

  if cbRequestTimestamp.Checked then
    FSigner.TimestampServer := editTSPServer.Text
  else
    FSigner.TimestampServer := '';

  FSigner.NewSignature.PolicyId := edIdentifier.Text;
  FSigner.NewSignature.PolicyHashAlgorithm := cbHashAlgorithm.Text;
  FSigner.NewSignature.PolicyHash := edHashValue.Text;

  FSigner.IgnoreChainValidationErrors := true;

  try
    FSigner.Sign();

    MessageDlg('The file(s) successfully signed', mtInformation, [mbOk], 0);
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormAsicsigner.FormCreate(Sender: TObject);
begin
  FSigner := TsbxASiCSigner.Create(nil);
end;

procedure TFormAsicsigner.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSigner);
end;

procedure TFormAsicsigner.sbOutputFileClick(Sender: TObject);
begin
  dlgSave.InitialDir := ExtractFilePath(Application.ExeName);
  if dlgSave.Execute then
    edOutputFile.Text := dlgSave.FileName;
end;

procedure TFormAsicsigner.sbSignCertFileClick(Sender: TObject);
begin
  dlgOpen.InitialDir := ExtractFilePath(Application.ExeName);
  dlgOpen.FileName := edSigningCertificate.Text;
  if dlgOpen.Execute then
    edSigningCertificate.Text := dlgOpen.FileName;
end;

end.
