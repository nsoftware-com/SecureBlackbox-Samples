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
unit soapverifierf;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Buttons, StdCtrls, ComCtrls,
  SBxTypes, SBxCore, SBxSOAPVerifier;

type
  TFormSoapverifier = class(TForm)
    lInputFile: TLabel;
    sbBrowseInputFile: TSpeedButton;
    lDemoInfo: TLabel;
    edInputFile: TEdit;
    btnVerify: TButton;
    dlgOpen: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure sbBrowseInputFileClick(Sender: TObject);
  private
    { Private declarations }
    FVerifier: TsbxSOAPVerifier;

    procedure DoReferenceValidated(Sender: TObject; ReferenceIndex: Integer; const Id: string;
      const Uri: string; const RefType: string; DigestValid: boolean);
    procedure DoSignatureFound(Sender: TObject; SigIndex : Integer; const EntityLabel: String;
      const IssuerRDN: string; const SerialNumber: TBytes; const SubjectKeyID: TBytes; CertFound: boolean;
      var ValidateSignature: boolean; var ValidateChain: boolean);
  public
    { Public declarations }
  end;

var
  FormSoapverifier: TFormSoapverifier;

implementation

{$R *.dfm}

uses uSignf, referencesf;

procedure TFormSoapverifier.DoReferenceValidated(Sender: TObject; ReferenceIndex: Integer;
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

procedure TFormSoapverifier.DoSignatureFound(Sender: TObject; SigIndex: Integer; const EntityLabel: String;
  const IssuerRDN: string; const SerialNumber: TBytes; const SubjectKeyID: TBytes; CertFound: boolean;
  var ValidateSignature: boolean; var ValidateChain: boolean);
var
  frmSign: TFormUsign;
begin
  ValidateChain := false; // this demo does not support certificate chain validation.

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

procedure TFormSoapverifier.btnVerifyClick(Sender: TObject);
var
  I : Integer;
begin
  try
    FormReferences.lvReferenceResults.Items.BeginUpdate;
    FormReferences.lvReferenceResults.Items.Clear;
    FormReferences.lvReferenceResults.Items.EndUpdate;

    FVerifier.KnownCertificates.Clear;

    FVerifier.InputFile := edInputFile.Text;

    FVerifier.Verify;

    if FVerifier.Signatures.Count = 0 then
      MessageDlg('No signatures found', mtError, [mbOk], 0)
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

procedure TFormSoapverifier.FormCreate(Sender: TObject);
begin
  FVerifier := TsbxSOAPVerifier.Create(nil);
  FVerifier.OnReferenceValidated := DoReferenceValidated;
  FVerifier.OnSignatureFound := DoSignatureFound;
end;

procedure TFormSoapverifier.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FVerifier);
end;

procedure TFormSoapverifier.sbBrowseInputFileClick(Sender: TObject);
begin
  dlgOpen.FileName := edInputFile.Text;
  dlgOpen.DefaultExt := 'xml';
  dlgOpen.Filter := 'XML and SOAP files (*.xml; *.soap)|*.xml; *.soap|All files|*.*';
  if dlgOpen.Execute then
    edInputFile.Text := dlgOpen.FileName;
end;

end.
