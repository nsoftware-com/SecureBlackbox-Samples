unit pdfprocessorf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StrUtils, StdCtrls, SBxTypes, SBxPDFSigner, SBxPDFVerifier;

type
  TFormPDFProcessor = class(TForm)
    edInputFile: TEdit;
    Label1: TLabel;
    btnSelectInput: TButton;
    Label2: TLabel;
    edOutputFile: TEdit;
    btnSelectOutput: TButton;
    btnSign: TButton;
    btnVerify: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    btnClose: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure btnSelectInputClick(Sender: TObject);
    procedure btnSelectOutputClick(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    FSigner: TSBxPDFSigner;
    FVerifier: TSBxPDFVerifier;

    procedure DoSignatureValidated(Sender: TObject; index: Integer; const EntityLabel: String;
      const IssuerRDN: String; const SerialNumber: TBytes; const SubjectKeyID: TBytes; ValidationResult: Integer;
      var Cancel: Boolean);
  public
    Cert: TSBxCertificate;
  end;

var
  FormPDFProcessor: TFormPDFProcessor;

implementation

{$R *.DFM}

uses validationresultf;

procedure TFormPDFProcessor.btnSelectInputClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edInputFile.Text := dlgOpen.FileName;
end;

procedure TFormPDFProcessor.btnSelectOutputClick(Sender: TObject);
begin
  if dlgSave.Execute then
    edOutputFile.Text := dlgSave.FileName;
end;

procedure TFormPDFProcessor.FormCreate(Sender: TObject);
begin
  FSigner := TSBxPDFSigner.Create(nil);
  FVerifier := TSBxPDFVerifier.Create(nil);
  FVerifier.OnSignatureValidated := DoSignatureValidated;
end;

procedure TFormPDFProcessor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSigner);
  FreeAndNil(FVerifier);
end;

procedure TFormPDFProcessor.btnSignClick(Sender: TObject);
begin
  FSigner.InputFile := edInputFile.Text;
  FSigner.OutputFile := edOutputFile.Text;

  FSigner.SigningCertificate := Cert;

  if ContainsText(FSigner.SigningCertificate.KeyAlgorithm, 'id-dsa') then
  begin
    MessageDlg('The certificate was found to contain a DSA key. The hash algorithm has been switched to SHA1.', mtInformation, [mbOk], 0);
    FSigner.NewSignature.HashAlgorithm := 'SHA1';
  end;

  FSigner.NewSignature.Level := paslBES;
  FSigner.Widget.Invisible := false;
  FSigner.IgnoreChainValidationErrors := true;

  try
    FSigner.Sign();

    MessageDlg('PDF file successfully signed', mtInformation, [mbOk], 0);
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;

  FSigner.SigningCertificate := nil;
end;

procedure TFormPDFProcessor.btnVerifyClick(Sender: TObject);
begin
  try
    FormValidationresult.Init(FVerifier);

    FVerifier.InputFile := edInputFile.Text;
    FVerifier.IgnoreChainValidationErrors := true;

    FVerifier.Verify;

    FormValidationresult.ShowModal;
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormPDFProcessor.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormPDFProcessor.DoSignatureValidated(Sender: TObject; index: Integer;
  const EntityLabel: String; const IssuerRDN: String; const SerialNumber: TBytes;
  const SubjectKeyID: TBytes; ValidationResult: Integer; var Cancel: Boolean);
begin
  FormValidationresult.cbSignatures.Items.AddObject(FVerifier.Signatures[index].SignatureName, FVerifier.Signatures[index].Clone);
end;

end.