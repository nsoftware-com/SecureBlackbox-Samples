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
    cbExtended: TCheckBox;
    cbHashAlgorithm: TComboBox;
    lHashAlgorithm: TLabel;
    cbRequestTimestamp: TCheckBox;
    editTSPServer: TEdit;
    procedure sbOutputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure sbSignCertFileClick(Sender: TObject);
    procedure bbClearClick(Sender: TObject);
    procedure btnBrowseInputFileClick(Sender: TObject);
  private
    { Private declarations }
    FSigner: TsbxASiCSigner;
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

procedure TFormAsicsigner.bbClearClick(Sender: TObject);
begin
  lbSourceFiles.Items.Clear;
end;

procedure TFormAsicsigner.btnBrowseInputFileClick(Sender: TObject);
begin
  dlgOpen.Filter := '';
  dlgOpen.Title := 'Please, select file';
  if dlgOpen.Execute then
    lbSourceFiles.Items.Add(dlgOpen.FileName);
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

  case cbLevel.ItemIndex of
    0: FSigner.NewSignature.Level := aslBaselineB;
    1: FSigner.NewSignature.Level := aslBaselineT;
    2: FSigner.NewSignature.Level := aslBaselineLT;
    3: FSigner.NewSignature.Level := aslBaselineLTA;
    4: FSigner.NewSignature.Level := aslBES;
    5: FSigner.NewSignature.Level := aslEPES;
    6: FSigner.NewSignature.Level := aslT;
    7: FSigner.NewSignature.Level := aslC;
    8: FSigner.NewSignature.Level := aslX;
    9: FSigner.NewSignature.Level := aslXType1;
    10: FSigner.NewSignature.Level := aslXType2;
    11: FSigner.NewSignature.Level := aslXL;
    12: FSigner.NewSignature.Level := aslXLType1;
    13: FSigner.NewSignature.Level := aslXLType2;
    14: FSigner.NewSignature.Level := aslA;
    15: FSigner.NewSignature.Level := aslExtendedBES;
    16: FSigner.NewSignature.Level := aslExtendedEPES;
    17: FSigner.NewSignature.Level := aslExtendedT;
    18: FSigner.NewSignature.Level := aslExtendedC;
    19: FSigner.NewSignature.Level := aslExtendedX;
    20: FSigner.NewSignature.Level := aslExtendedXType1;
    21: FSigner.NewSignature.Level := aslExtendedXType2;
    22: FSigner.NewSignature.Level := aslExtendedXLong;
    23: FSigner.NewSignature.Level := aslExtendedXL;
    24: FSigner.NewSignature.Level := aslExtendedXLType1;
    25: FSigner.NewSignature.Level := aslExtendedXLType2;
    26: FSigner.NewSignature.Level := aslExtendedA;
  end;

  case cbSignatureType.ItemIndex of
    0: FSigner.NewSignature.SignatureType := castCAdES;
    1: FSigner.NewSignature.SignatureType := castXAdES;
  end;

  FSigner.NewSignature.HashAlgorithm := cbHashAlgorithm.Text;

  FSigner.Extended := cbExtended.Checked;

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

  FSigner.NewSignature.PolicyId := '1.2.3.4.5';

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
