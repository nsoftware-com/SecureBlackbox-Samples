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
unit officeencryptorf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxOfficeEncryptor;

type
  TFormOfficeencryptor = class(TForm)
    lDemoInfo: TLabel;
    lbInputFile: TLabel;
    btnBrowseInput: TButton;
    lOutputFile: TLabel;
    btnBrowseOutput: TButton;
    edInputFile: TEdit;
    edOutputFile: TEdit;
    gbEncryptionOptions: TGroupBox;
    btnEncrypt: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    cbEncryptionType: TComboBox;
    Label4: TLabel;
    lEncryptionAlgorithm: TLabel;
    cbEncryptionAlgorithm: TComboBox;
    GroupBox1: TGroupBox;
    cbEncryptDocumentProperties: TCheckBox;
    cbHardenedKeyGeneration: TCheckBox;
    lPassword: TLabel;
    edPassword: TEdit;
    lRC4KeyBits: TLabel;
    edRC4KeyBits: TEdit;
    GroupBox2: TGroupBox;
    lSaltSize: TLabel;
    edSaltSize: TEdit;
    lSpinCount: TLabel;
    edSpinCount: TEdit;
    lHashAlgorithm: TLabel;
    cbHashALgorithm: TComboBox;
    GroupBox3: TGroupBox;
    lChecksumAlgorithm: TLabel;
    cbChecksumAlgorithm: TComboBox;
    procedure btnBrowseInputClick(Sender: TObject);
    procedure btnBrowseOutputClick(Sender: TObject);
    procedure btnEncryptClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormOfficeencryptor: TFormOfficeencryptor;

implementation

{$R *.dfm}

procedure TFormOfficeencryptor.btnEncryptClick(Sender: TObject);
var
  FEncryptor : TsbxOfficeEncryptor;
begin
  FEncryptor := TsbxOfficeEncryptor.Create(nil);
  try
    FEncryptor.InputFile := edInputFile.Text;
    FEncryptor.OutputFile := edOutputFile.Text;

    case cbEncryptionType.ItemIndex of
      0: FEncryptor.EncryptionType := oetDefault;
      1: FEncryptor.EncryptionType := oetBinaryRC4;
      2: FEncryptor.EncryptionType := oetBinaryRC4CryptoAPI;
      3: FEncryptor.EncryptionType := oetOpenXMLStandard;
      4: FEncryptor.EncryptionType := oetOpenXMLAgile;
      5: FEncryptor.EncryptionType := oetOpenDocument;
    end;

    if cbEncryptionAlgorithm.ItemIndex > 0 then
      FEncryptor.EncryptionAlgorithm := cbEncryptionAlgorithm.Text;

    FEncryptor.Password := edPassword.Text;

    // RC4 Crypto API additional settings
    if cbEncryptDocumentProperties.Checked then
      FEncryptor.Config('EncryptDocumentProperties=true')
    else
      FEncryptor.Config('EncryptDocumentProperties=false');

    if cbHardenedKeyGeneration.Checked then
      FEncryptor.Config('HardenedKeyGeneration=true')
    else
      FEncryptor.Config('HardenedKeyGeneration=false');

    if StrToIntDef(edRC4KeyBits.Text, -1) > 0 then
      FEncryptor.Config('RC4KeyBits=' + edRC4KeyBits.Text);

    // OpenXML Agile encryption additional settings
    if StrToIntDef(edSaltSize.Text, -1) > 0 then
      FEncryptor.Config('SaltSize=' + edSaltSize.Text);

    if StrToIntDef(edSpinCount.Text, -1) > 0 then
      FEncryptor.Config('SpinCount=' + edSpinCount.Text);

    FEncryptor.Config('HashAlgorithm=' + cbHashAlgorithm.Text);

    // OpenOffice additional settings
    FEncryptor.Config('ChecksumAlgorithm=' + cbChecksumAlgorithm.Text);
    FEncryptor.Config('StartKeyGenerationAlgorithm=' + cbChecksumAlgorithm.Text);

    try
      FEncryptor.Encrypt();

      MessageDlg('Office document successfully encrypted', mtInformation, [mbOk], 0);

      Close;
    except
      on E: Exception do
        MessageDlg(E.message, mtError, [mbOk], 0);
    end;
  finally
    FreeAndNil(FEncryptor);
  end;
end;

procedure TFormOfficeencryptor.btnBrowseInputClick(Sender: TObject);
begin
  dlgOpen.FileName := edInputFile.Text;
  dlgOpen.Filter := 'All Documents|*.doc; *.dot; *.docx; *.docm; *.dotx; *.dotm; *.xls; *.xlt; *.xlm; *.xlsx; *.xlsm; *.xlsb; *.xltx; *.xltm;' + ' *.ppt; *.ppm; *.pot; *.pps; *.pptx; *.pptm; *.potm; *.ppsx; *.ppsm; *.xps; *.odt; *.ott; *.odg; *.otg; *.odp; *.otp; *.ods; *.ots; *.odc; *.otc; *.odi; *.oti; *.odf; *.otf; *.odm; *.oth; *.odb|' +
    'All files (*.*)|*.*|' +
    'Word Document (*.doc; *.dot; *.docx; *.docm; *.dotx; *.dotm)|*.doc; *.dot; *.docx; *.docm; *.dotx; *.dotm|' +
    'Excel (*.xls; *.xlt; *.xlm; *.xlsx; *.xlsm; *.xlsb; *.xltx; *.xltm)|*.xls; *.xlt; *.xlm; *.xlsx; *.xlsm; *.xlsb; *.xltx; *.xltm|' +
    'PowerPoint (*.ppt; *.ppm; *.pot; *.pps; *.pptx; *.pptm; *.potm; *.ppsx; *.ppsm)|*.ppt; *.ppm; *.pot; *.pps; *.pptx; *.pptm; *.potm; *.ppsx; *.ppsm|' +
    'Open XPS (*.xps)|*.xps|' +
    'OpenDocument Format|*.odt; *.ott; *.odg; *.otg; *.odp; *.otp; *.ods; *.ots; *.odc; *.otc; *.odi; *.oti; *.odf; *.otf; *.odm; *.oth; *.odb';

  if dlgOpen.Execute then
    edInputFile.Text := dlgOpen.FileName;
end;

procedure TFormOfficeencryptor.btnBrowseOutputClick(Sender: TObject);
begin
  dlgSave.InitialDir := ExtractFilePath(edInputFile.Text);
  dlgSave.FileName := edOutputFile.Text;
  dlgSave.Filter := 'Current Document Format|*' + ExtractFileExt(edInputFile.Text);

  if dlgSave.Execute then
    edOutputFile.Text := dlgSave.FileName;
end;

end.


