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
    cbLevel: TComboBox;
    lbHashAlgorithm: TLabel;
    cmbHashAlgorithm: TComboBox;
    cbDetached: TCheckBox;
    cbRequestTimestamp: TCheckBox;
    editTSPServer: TEdit;
    procedure sbBrowseInputFileClick(Sender: TObject);
    procedure sbOutputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure sbSignCertFileClick(Sender: TObject);
  private
    { Private declarations }
    FSigner: TsbxCAdESSigner;
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

procedure TFormCadessigner.btnSignClick(Sender: TObject);
var
  CertificateManager: TsbxCertificateManager;
begin
  FSigner.InputFile := edInputFile.Text;
  FSigner.OutputFile := edOutputFile.Text;

  FSigner.Detached := cbDetached.Checked;
  FSigner.NewSignature.Level := TsbxAdESSignatureLevels(cbLevel.Items.Objects[cbLevel.ItemIndex]);
  FSigner.NewSignature.HashAlgorithm := cmbHashAlgorithm.Text;

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
    FSigner.Sign();

    MessageDlg('The file successfully signed', mtInformation, [mbOk], 0);
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormCadessigner.FormCreate(Sender: TObject);
begin
  FSigner := TsbxCAdESSigner.Create(nil);

  cbLevel.Items.AddObject('BaselineB', TObject(aslBaselineB));
  cbLevel.Items.AddObject('BaselineT', TObject(aslBaselineT));
  cbLevel.Items.AddObject('BaselineLT', TObject(aslBaselineLT));
  cbLevel.Items.AddObject('BaselineLTA', TObject(aslBaselineLTA));
  cbLevel.Items.AddObject('BES', TObject(aslBES));
  cbLevel.Items.AddObject('EPES', TObject(aslEPES));
  cbLevel.Items.AddObject('T', TObject(aslT));
  cbLevel.Items.AddObject('C', TObject(aslC));
  cbLevel.Items.AddObject('X', TObject(aslX));
  cbLevel.Items.AddObject('XType1', TObject(aslXType1));
  cbLevel.Items.AddObject('XType2', TObject(aslXType2));
  cbLevel.Items.AddObject('XL', TObject(aslXL));
  cbLevel.Items.AddObject('XLType1', TObject(aslXLType1));
  cbLevel.Items.AddObject('XLType2', TObject(aslXLType2));
  cbLevel.Items.AddObject('A', TObject(aslA));
  cbLevel.Items.AddObject('ExtendedBES', TObject(aslExtendedBES));
  cbLevel.Items.AddObject('ExtendedEPES', TObject(aslExtendedEPES));
  cbLevel.Items.AddObject('ExtendedT', TObject(aslExtendedT));
  cbLevel.Items.AddObject('ExtendedC', TObject(aslExtendedC));
  cbLevel.Items.AddObject('ExtendedX', TObject(aslExtendedX));
  cbLevel.Items.AddObject('ExtendedXType1', TObject(aslExtendedXType1));
  cbLevel.Items.AddObject('ExtendedXType2', TObject(aslExtendedXType2));
  cbLevel.Items.AddObject('ExtendedXLong', TObject(aslExtendedXLong));
  cbLevel.Items.AddObject('ExtendedXL', TObject(aslExtendedXL));
  cbLevel.Items.AddObject('ExtendedXLType1', TObject(aslExtendedXLType1));
  cbLevel.Items.AddObject('ExtendedXLType2', TObject(aslExtendedXLType2));
  cbLevel.Items.AddObject('ExtendedA', TObject(aslExtendedA));

  cbLevel.ItemIndex := 0;
  cmbHashAlgorithm.ItemIndex := 2;
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






