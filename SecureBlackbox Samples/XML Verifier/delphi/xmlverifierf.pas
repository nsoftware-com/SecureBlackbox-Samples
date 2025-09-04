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
unit xmlverifierf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxXMLVerifier;

type
  TFormXmlverifier = class(TForm)
    lbXMLFile: TLabel;
    sbBrowseInputFile: TSpeedButton;
    lDataFile: TLabel;
    sbBrowseDataFile: TSpeedButton;
    Label10: TLabel;
    cbDetached: TCheckBox;
    edInputFile: TEdit;
    btnVerify: TButton;
    edDataFile: TEdit;
    dlgOpen: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure cbDetachedClick(Sender: TObject);
    procedure sbBrowseInputFileClick(Sender: TObject);
    procedure sbBrowseDataFileClick(Sender: TObject);
  private
    { Private declarations }
    FVerifier: TsbxXMLVerifier;

    procedure DoReferenceValidated(Sender: TObject; ReferenceIndex: Integer;
      const Id: string; const Uri: string; const RefType: string; DigestValid: boolean);
    procedure DoSignatureFound(Sender: TObject; SigIndex: Integer; const EntityLabel: String;
      const IssuerRDN: string; const SerialNumber: TBytes; const SubjectKeyID: TBytes;
      CertFound: boolean; var ValidateSignature: boolean; var ValidateChain: boolean);
  public
    { Public declarations }
  end;

var
  FormXmlverifier: TFormXmlverifier;

implementation

{$R *.dfm}

uses uSignf, referencesf;

procedure TFormXmlverifier.DoReferenceValidated(Sender: TObject; ReferenceIndex: Integer;
  const Id: string; const Uri: string; const RefType: string; DigestValid: boolean);
var
  Item: TListItem;
begin
  FormReferences.lvReferenceResults.Items.BeginUpdate;

  Item := FormReferences.lvReferenceResults.Items.Add;
  Item.Caption := Id;
  Item.SubItems.Add(Uri);
  if DigestValid then
    Item.SubItems.Add('true')
  else
    Item.SubItems.Add('false');

  FormReferences.lvReferenceResults.Items.EndUpdate;
end;

procedure TFormXmlverifier.DoSignatureFound(Sender: TObject; SigIndex: Integer;
  const EntityLabel: String; const IssuerRDN: string; const SerialNumber: TBytes; const SubjectKeyID: TBytes;
  CertFound: boolean; var ValidateSignature: boolean; var ValidateChain: boolean);
var
  frmSign: TFormUsign;
begin
  ValidateChain := false; // XMLVerfier control does not support certificate chain validation.

  if CertFound then
    ValidateSignature := true
  else
  begin
    frmSign := TFormUsign.Create(nil);
    try
      frmSign.SetSignature(FVerifier, SigIndex);

      if frmSign.ShowModal = mrOK then
        ValidateSignature := true
      else
        ValidateSignature := false;
    finally
      FreeAndNil(frmSign);
    end;
  end;
end;

procedure TFormXmlverifier.btnVerifyClick(Sender: TObject);
var
  I : Integer;
begin
  try
    FormReferences.lvReferenceResults.Items.BeginUpdate;
    FormReferences.lvReferenceResults.Items.Clear;
    FormReferences.lvReferenceResults.Items.EndUpdate;

    FVerifier.KnownCertificates.Clear;

    FVerifier.InputFile := edInputFile.Text;

    if cbDetached.Checked then
    begin
      FVerifier.DataFile := edDataFile.Text;
      FVerifier.DataType := cxdtBinary;
      FVerifier.DataURI := ExtractFileName(edDataFile.Text);
      FVerifier.VerifyDetached()
    end
    else
      FVerifier.Verify();

    if FVerifier.Signatures.Count = 0 then
      MessageDlg('Signature not found', mtError, [mbOk], 0)
    else
    begin
      for I := 0 to FVerifier.Signatures.Count - 1 do
      begin
        case FVerifier.Signatures[I].SignatureValidationResult of
          svtSignerNotFound:
            MessageDlg('Signer not found', mtError, [mbOk], 0);
          svtFailure:
            MessageDlg('Signature verification failed', mtError, [mbOk], 0);
          svtCorrupted:
            MessageDlg('Signature is invalid', mtError, [mbOk], 0);
          svtReferenceCorrupted:
            if MessageDlg('Signature has invalid references.'#13#10'Do you want to view reference validation results?', mtError, [mbYes, mbNo], 0) = mrYes then
              FormReferences.ShowModal;
          svtValid:
            if MessageDlg('Signature validated successfully.'#13#10'Do you want to view reference validation results?', mtInformation, [mbYes, mbNo], 0) = mrYes then
              FormReferences.ShowModal;
          else
            MessageDlg('Unknown signature validation result', mtInformation, [mbOk], 0);
        end;
      end;
    end;
  except
    on E: Exception do
      MessageDlg(e.message, mtError, [mbOk], 0);
  end;
end;

procedure TFormXmlverifier.cbDetachedClick(Sender: TObject);
begin
  lDataFile.Enabled := cbDetached.Checked;
  edDataFile.Enabled := cbDetached.Checked;
  sbBrowseDataFile.Enabled := cbDetached.Checked;
end;

procedure TFormXmlverifier.FormCreate(Sender: TObject);
begin
  FVerifier := TsbxXMLVerifier.Create(nil);
  FVerifier.OnReferenceValidated := DoReferenceValidated;
  FVerifier.OnSignatureFound := DoSignatureFound;
  lDataFile.Enabled := cbDetached.Checked;
end;

procedure TFormXmlverifier.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FVerifier);
end;

procedure TFormXmlverifier.sbBrowseDataFileClick(Sender: TObject);
begin
  dlgOpen.Filter := 'All files (*.*)|*.*';
  dlgOpen.FileName := edDataFile.Text;
  if dlgOpen.Execute then
    edDataFile.Text := dlgOpen.FileName;
end;

procedure TFormXmlverifier.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpen.Filter := 'XML files (*.xml)|*.xml|' +
    'All files (*.*)|*.*';
  dlgOpen.FileName := edInputFile.Text;
  if dlgOpen.Execute then
    edInputFile.Text := dlgOpen.FileName;
end;

end.
