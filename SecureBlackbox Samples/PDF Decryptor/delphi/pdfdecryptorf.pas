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
unit pdfdecryptorf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, pdfencpropsf,
  SBxPDFDecryptor;

type
  TFormPdfdecryptor = class(TForm)
    lSourceFile: TLabel;
    editSource: TEdit;
    lDestFile: TLabel;
    editDest: TEdit;
    btnBrowseSource: TButton;
    btnBrowseDest: TButton;
    btnDecrypt: TButton;
    btnCancel: TButton;
    OpenDialogPDF: TOpenDialog;
    SaveDialogPDF: TSaveDialog;
    lDemoInfo: TLabel;
    procedure btnBrowseSourceClick(Sender: TObject);
    procedure btnBrowseDestClick(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    procedure HandleEncrypted(Sender: TObject; CertUsed: boolean; const IssuerRDN: string; SerialNumber: TBytes; SubjectKeyID: TBytes; NeedCredential: boolean; var SkipThis: Boolean);
  public
    { Public declarations }
  end;

var
  FormPdfdecryptor: TFormPdfdecryptor;

implementation

{$R *.DFM}

procedure TFormPdfdecryptor.btnBrowseSourceClick(Sender: TObject);
begin
  OpenDialogPDF.Filename := editSource.Text;
  OpenDialogPDF.Filter := 'PDF files (*.pdf)|*.pdf|All files (*.*)|*.*';
  if OpenDialogPDF.Execute then
    editSource.Text := OpenDialogPDF.Filename;
end;

procedure TFormPdfdecryptor.btnBrowseDestClick(Sender: TObject);
begin
  SaveDialogPDF.FileName := editDest.Text;
  SaveDialogPDF.Filter := 'PDF files (*.pdf)|*.pdf|All files (*.*)|*.*';
  if SaveDialogPDF.Execute then
    editDest.Text := SaveDialogPDF.FileName;
end;

procedure TFormPdfdecryptor.HandleEncrypted(Sender: TObject; CertUsed: boolean; const IssuerRDN: string; SerialNumber: TBytes; SubjectKeyID: TBytes; NeedCredential: boolean; var SkipThis: Boolean);
begin
  FormPdfencprops.SetPDFEncryptionProps(Sender as TsbxPDFDecryptor);

  SkipThis := (FormPdfencprops.ShowModal = mrCancel);

  FormPdfencprops.GetPDFDecryptionInfo(Sender as TsbxPDFDecryptor);
end;

procedure TFormPdfdecryptor.btnDecryptClick(Sender: TObject);
var
  PDFDecryptor : TsbxPDFDecryptor;
begin
  PDFDecryptor := TsbxPDFDecryptor.Create(nil);
  try
    try
      PDFDecryptor.OnEncrypted := HandleEncrypted;

      PDFDecryptor.InputFile := editSource.Text;
      PDFDecryptor.OutputFile := editDest.Text;

      PDFDecryptor.Decrypt();

      MessageDlg('PDF file successfully decrypted', mtInformation, [mbOk], 0);
    except
      on E: Exception do
        MessageDlg(e.Message, mtError, [mbOk], 0);
    end;
  finally
    FreeAndNil(PDFDecryptor);
  end;

  Close();
end;

procedure TFormPdfdecryptor.btnCancelClick(Sender: TObject);
begin
  Close();
end;

end.



