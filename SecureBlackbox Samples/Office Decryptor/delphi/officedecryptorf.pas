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
unit officedecryptorf;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, officeencpropsf,
  SBxTypes, SBxCore, SBxOfficeDecryptor;

type
  TFormOfficedecryptor = class(TForm)
    lSourceFile: TLabel;
    edSource: TEdit;
    lDestFile: TLabel;
    edDest: TEdit;
    btnBrowseSource: TButton;
    btnBrowseDest: TButton;
    btnDecrypt: TButton;
    btnCancel: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    lDemoInfo: TLabel;
    procedure btnBrowseSourceClick(Sender: TObject);
    procedure btnBrowseDestClick(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    procedure HandleDecryptionPasswordNeeded(Sender: TObject; var CancelDecryption: Boolean);
  public
    { Public declarations }
  end;

var
  FormOfficedecryptor: TFormOfficedecryptor;

implementation

{$R *.DFM}

procedure TFormOfficedecryptor.btnBrowseSourceClick(Sender: TObject);
begin
  dlgOpen.Filename := edSource.Text;
  dlgOpen.Filter := 'All Documents|*.doc; *.dot; *.docx; *.docm; *.dotx; *.dotm; *.xls; *.xlt; *.xlm; *.xlsx; *.xlsm; *.xlsb; *.xltx; *.xltm;' + ' *.ppt; *.ppm; *.pot; *.pps; *.pptx; *.pptm; *.potm; *.ppsx; *.ppsm; *.xps; *.odt; *.ott; *.odg; *.otg; *.odp; *.otp; *.ods; *.ots; *.odc; *.otc; *.odi; *.oti; *.odf; *.otf; *.odm; *.oth; *.odb|' +
    'All files (*.*)|*.*|' +
    'Word Document (*.doc; *.dot; *.docx; *.docm; *.dotx; *.dotm)|*.doc; *.dot; *.docx; *.docm; *.dotx; *.dotm|' +
    'Excel (*.xls; *.xlt; *.xlm; *.xlsx; *.xlsm; *.xlsb; *.xltx; *.xltm)|*.xls; *.xlt; *.xlm; *.xlsx; *.xlsm; *.xlsb; *.xltx; *.xltm|' +
    'PowerPoint (*.ppt; *.ppm; *.pot; *.pps; *.pptx; *.pptm; *.potm; *.ppsx; *.ppsm)|*.ppt; *.ppm; *.pot; *.pps; *.pptx; *.pptm; *.potm; *.ppsx; *.ppsm|' +
    'Open XPS (*.xps)|*.xps|' +
    'OpenDocument Format|*.odt; *.ott; *.odg; *.otg; *.odp; *.otp; *.ods; *.ots; *.odc; *.otc; *.odi; *.oti; *.odf; *.otf; *.odm; *.oth; *.odb';

  if dlgOpen.Execute then
    edSource.Text := dlgOpen.Filename;
end;

procedure TFormOfficedecryptor.btnBrowseDestClick(Sender: TObject);
begin
  dlgSave.InitialDir := ExtractFilePath(edSource.Text);
  dlgSave.FileName := edDest.Text;
  dlgSave.Filter := 'Current Document Format|*' + ExtractFileExt(edSource.Text);

  if dlgSave.Execute then
    edDest.Text := dlgSave.FileName;
end;

procedure TFormOfficedecryptor.HandleDecryptionPasswordNeeded(Sender: TObject; var CancelDecryption: Boolean);
begin
  FormOfficeencprops.SetOfficeEncryptionProps(Sender as TsbxOfficeDecryptor);

  CancelDecryption := (FormOfficeencprops.ShowModal = mrCancel);

  FormOfficeencprops.GetOfficeDecryptionInfo(Sender as TsbxOfficeDecryptor);
end;

procedure TFormOfficedecryptor.btnDecryptClick(Sender: TObject);
var
  OfficeDecryptor : TsbxOfficeDecryptor;
begin
  OfficeDecryptor := TsbxOfficeDecryptor.Create(nil);
  try
    try
      OfficeDecryptor.OnDecryptionPasswordNeeded := HandleDecryptionPasswordNeeded;

      OfficeDecryptor.InputFile := edSource.Text;
      OfficeDecryptor.OutputFile := edDest.Text;

      OfficeDecryptor.Decrypt();

      MessageDlg('Office file successfully decrypted', mtInformation, [mbOk], 0);
    except
      on E: Exception do
        MessageDlg(e.Message, mtError, [mbOk], 0);
    end;
  finally
    FreeAndNil(OfficeDecryptor);
  end;

  Close();
end;

procedure TFormOfficedecryptor.btnCancelClick(Sender: TObject);
begin
  Close();
end;

end.



