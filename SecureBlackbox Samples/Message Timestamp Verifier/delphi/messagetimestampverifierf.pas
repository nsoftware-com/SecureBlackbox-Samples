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
unit messagetimestampverifierf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxCertificateManager, SBxMessageTimestampVerifier;

type
  TFormMessagetimestampverifier = class(TForm)
    lbXMLFile: TLabel;
    sbBrowseInputFile: TSpeedButton;
    lOutputFile: TLabel;
    sbBrowseOutputFile: TSpeedButton;
    Label10: TLabel;
    cbDetached: TCheckBox;
    edInputFile: TEdit;
    btnVerify: TButton;
    edOutputFile: TEdit;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    procedure sbBrowseInputFileClick(Sender: TObject);
    procedure sbBrowseOutputFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure cbDetachedClick(Sender: TObject);
  private
    { Private declarations }
    FVerifier: TsbxMessageTimestampVerifier;

    function WriteCertificateInfo(CertList: TsbxCertificateList): string;
  public
    { Public declarations }
  end;

var
  FormMessagetimestampverifier: TFormMessagetimestampverifier;

implementation

{$R *.dfm}

uses resultsf;

function BinaryToString(const Buffer : TBytes): string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to Length(Buffer) - 1 do
    Result := Result + IntToHex(Buffer[i], 1);
end;

function TFormMessagetimestampverifier.WriteCertificateInfo(CertList: TsbxCertificateList) : string;
var
  i: integer;
begin
  for i := 0 to CertList.Count - 1 do
  begin
    Result := Result + 'Certificate #' + IntToStr(i + 1) + ':'#13#10;

    Result := Result + 'Issuer: ' + CertList.Item[i].Issuer + #13#10;
    Result := Result + 'Subject: ' + CertList.Item[i].Subject + #13#10;

    if CertList.Item[i].PrivateKeyExists then
      Result := Result + 'Private key available'#13#10#13#10
    else
      Result := Result + 'Private key is not available'#13#10#13#10;
  end;
end;

procedure TFormMessagetimestampverifier.btnVerifyClick(Sender: TObject);
var
  SigOK: boolean;
begin
  try
    FVerifier.InputFile := edInputFile.Text;

    if cbDetached.Checked then
    begin
      FVerifier.DataFile := edOutputFile.Text;
      FVerifier.VerifyDetached;
    end
    else
    begin
      FVerifier.OutputFile := edOutputFile.Text;
      FVerifier.Verify;
    end;

    FormResults.mResults.Clear;
    case FVerifier.SignatureValidationResult of
      svtCorrupted:
        FormResults.mResults.Lines.Add('Verification error: Corrupted');
      svtFailure:
        FormResults.mResults.Lines.Add('Verification error: Failure');
      svtSignerNotFound:
        FormResults.mResults.Lines.Add('Verification error: SignerNotFound');
      svtUnknown:
        FormResults.mResults.Lines.Add('Verification error: Unknown');
      else
      begin
        FormResults.mResults.Lines.Add('Successfully verified!');
        FormResults.mResults.Lines.Add('');
        FormResults.mResults.Lines.Add('Validated signing time: ' + FVerifier.ValidatedSigningTime);
        FormResults.mResults.Lines.Add('');
        FormResults.mResults.Lines.Add('Certificates contained in message:');
        FormResults.mResults.Lines.Add(WriteCertificateInfo(FVerifier.Certificates));

      end;
    end;

    FormResults.ShowModal;
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormMessagetimestampverifier.cbDetachedClick(Sender: TObject);
begin
  if cbDetached.Checked then
    lOutputFile.Caption := 'Data file:'
  else
    lOutputFile.Caption := 'Output file:';
end;

procedure TFormMessagetimestampverifier.FormCreate(Sender: TObject);
begin
  FVerifier := TsbxMessageTimestampVerifier.Create(nil);
end;

procedure TFormMessagetimestampverifier.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FVerifier);
end;

procedure TFormMessagetimestampverifier.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edInputFile.Text;
  dlgOpen.Filter := '';
  if dlgOpen.Execute then
    edInputFile.Text := dlgOpen.FileName;
end;

procedure TFormMessagetimestampverifier.sbBrowseOutputFileClick(Sender: TObject);
begin
  if cbDetached.Checked then
  begin
    dlgOpen.FileName := edOutputFile.Text;
    dlgOpen.Filter := '';
    if dlgOpen.Execute then
      edOutputFile.Text := dlgOpen.FileName;
  end
  else
  begin
    dlgSave.FileName := edOutputFile.Text;
    if dlgSave.Execute then
      edOutputFile.Text := dlgSave.FileName;
  end;
end;

end.







