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
unit cadessignerf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxCertificateManager, SBxCAdESSigner;

type
  TFormCadessigner = class(TForm)
    lbInputFile: TLabel;
    sbBrowseInputFile: TSpeedButton;
    Label1: TLabel;
    sbOutputFile: TSpeedButton;
    edInputFile: TEdit;
    edOutputFile: TEdit;
    btnSign: TButton;
    dlgOpenFile: TOpenDialog;
    dlgSaveFile: TSaveDialog;
    DlgOpenCert: TOpenDialog;
    Label10: TLabel;
    GroupBox6: TGroupBox;
    Label4: TLabel;
    Label2: TLabel;
    sbSignCertFile: TSpeedButton;
    Label3: TLabel;
    edSigningCertificate: TEdit;
    edCertPassword: TEdit;
    GroupBox5: TGroupBox;
    lvSigningCertificates: TListView;
    btnRemoveCert: TButton;
    btnAddCert: TButton;
    cbLevel: TComboBox;
    lbHashAlgorithm: TLabel;
    cmbHashAlgorithm: TComboBox;
    cbDetached: TCheckBox;
    GroupBox1: TGroupBox;
    cbRequestTimestamp: TCheckBox;
    editTSPServer: TEdit;
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
    FSigner: TsbxCAdESSigner;

    procedure UpdateCertificates;
  public
    { Public declarations }
  end;

var
  FormCadessigner: TFormCadessigner;

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

procedure TFormCadessigner.UpdateCertificates;
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

procedure TFormCadessigner.btnAddCertClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
begin
  DlgOpenCert.Title := 'Select certificate file';
  DlgOpenCert.Filter := 'PEM-encoded certificate (*.pem)|*.PEM|DER-encoded certificate (*.cer)|*.CER|PFX-encoded certificate (*.pfx)|*.PFX';
  if DlgOpenCert.Execute then
  begin
    CertificateManager := TsbxCertificateManager.Create(nil);
    try
      try
        CertificateManager.ImportFromFile(DlgOpenCert.Filename, InputBox('Please enter passphrase:', '',''));

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

procedure TFormCadessigner.btnRemoveCertClick(Sender: TObject);
begin
  if Assigned(lvSigningCertificates.Selected) then
  begin
    FSigner.SigningChain.RemoveAt(lvSigningCertificates.Selected.Index);
    UpdateCertificates;
  end;
end;

procedure TFormCadessigner.btnSignClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
begin
  FSigner.InputFile := edInputFile.Text;
  FSigner.OutputFile := edOutputFile.Text;

  FSigner.HashAlgorithm := cmbHashAlgorithm.Text;

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

  if cbRequestTimestamp.Checked then
    FSigner.TimestampServer := editTSPServer.Text
  else
    FSigner.TimestampServer := '';

  try
    FSigner.Sign(cbLevel.ItemIndex + 1, cbDetached.Checked);

    MessageDlg('The file successfully signed', mtInformation, [mbOk], 0);
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormCadessigner.FormCreate(Sender: TObject);
begin
  FSigner := TsbxCAdESSigner.Create(nil);

  cbLevel.ItemIndex := 0;
  cmbHashAlgorithm.ItemIndex := 0;
end;

procedure TFormCadessigner.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSigner);
end;

procedure TFormCadessigner.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpenFile.FileName := edInputFile.Text;
  if dlgOpenFile.Execute then
    edInputFile.Text := dlgOpenFile.FileName;
end;

procedure TFormCadessigner.sbOutputFileClick(Sender: TObject);
begin
  if dlgSaveFile.Execute then
    edOutputFile.Text := dlgSaveFile.FileName;
end;

procedure TFormCadessigner.sbSignCertFileClick(Sender: TObject);
begin
  DlgOpenCert.FileName := edSigningCertificate.Text;
  if DlgOpenCert.Execute then
    edSigningCertificate.Text := DlgOpenCert.FileName;
end;

end.






