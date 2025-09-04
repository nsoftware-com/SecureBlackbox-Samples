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
unit samlreaderf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, sbxcertificatemanager,
  sbxsamlreader, sbxtypes, sbxconstants;

type
  TFormSAMLReader = class(TForm)
    gbSecurity: TGroupBox;
    lblEncCert: TLabel;
    lblSigningCert: TLabel;
    lblCertPassword: TLabel;
    editDecryptionCert: TEdit;
    btnBrowseEncCert: TButton;
    editVerifyingCert: TEdit;
    btnBrowseSigningCert: TButton;
    editPassword: TEdit;
    dlgVerifyingCert: TOpenDialog;
    dlgDecryptionCert: TOpenDialog;
    gbProcess: TGroupBox;
    memoInput: TMemo;
    btnGo: TButton;
    memoOutput: TMemo;
    procedure btnBrowseSigningCertClick(Sender: TObject);
    procedure btnBrowseEncCertClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
  private
    procedure HandleReaderEncrypted(Sender: TObject; const IssuerRDN: String;
      const SerialNumber: TBytes; const SubjectKeyID: TBytes; NeedCredential: Boolean;
      var SkipThis: Boolean);
    procedure HandleReaderSignatureFound(Sender: TObject; Scope: Integer;
      const IssuerRDN: String; const SerialNumber: TBytes; const SubjectKeyID: TBytes;
      CertFound: Boolean; var Validate: Boolean);
    procedure HandleReaderSignatureValidated(Sender: TObject; Scope: Integer;
      const IssuerRDN: String; const SerialNumber: TBytes; const SubjectKeyID: TBytes;
      var ValidationResult: Integer);
    procedure OutputSubjectQuery(Reader: TsbxSAMLReader);
    procedure OutputAttributeQuery(Reader: TsbxSAMLReader);
    procedure OutputAuthnRequest(Reader: TsbxSAMLReader);
    procedure OutputLogoutRequest(Reader: TsbxSAMLReader);
    procedure OutputResponse(Reader: TsbxSAMLReader);
    procedure OutputAssertion(Reader: TsbxSAMLReader);
    procedure OutputSubjectConfirmations(Reader: TsbxSAMLReader);
    procedure OutputAttributes(Reader: TsbxSAMLReader);
    procedure OutputConditions(Reader: TsbxSAMLReader);
    procedure OutputStatements(Reader: TsbxSAMLReader);
    procedure Output(const S : string);
  public
    { Public declarations }
  end;

var
  FormSAMLReader: TFormSAMLReader;

implementation

{$R *.dfm}

procedure TFormSAMLReader.btnBrowseEncCertClick(Sender: TObject);
begin
  if dlgDecryptionCert.Execute then
    editDecryptionCert.Text := dlgDecryptionCert.FileName;
end;

procedure TFormSAMLReader.btnBrowseSigningCertClick(Sender: TObject);
begin
  if dlgVerifyingCert.Execute then
    editVerifyingCert.Text := dlgVerifyingCert.FileName;
end;

procedure TFormSAMLReader.btnGoClick(Sender: TObject);
var
  VerCertMgr, DecCertMgr : TsbxCertificateManager;
  Reader : TsbxSAMLReader;
begin
  memoOutput.Lines.Text := '';

  VerCertMgr := nil;
  DecCertMgr := nil;

  try
    try
      // loading the signing certificate
      if Length(editVerifyingCert.Text) > 0 then
      begin
        VerCertMgr := TsbxCertificateManager.Create(nil);
        VerCertMgr.ImportFromFile(editVerifyingCert.Text, '');
      end;

      // loading the encryption certificate
      if Length(editDecryptionCert.Text) > 0 then
      begin
        DecCertMgr := TsbxCertificateManager.Create(nil);
        DecCertMgr.ImportFromFile(editDecryptionCert.Text, editPassword.Text);
      end;

      // processing the message
      Reader := TsbxSAMLReader.Create(nil);
      try
        // setting up some events
        Reader.OnEncrypted := HandleReaderEncrypted;
        Reader.OnSignatureFound := HandleReaderSignatureFound;
        Reader.OnSignatureValidated := HandleReaderSignatureValidated;

        // setting up certificates
        if DecCertMgr <> nil then
          Reader.DecryptionCertificate := DecCertMgr.Certificate;
        if VerCertMgr <> nil then
          Reader.Certificates.Add(VerCertMgr.Certificate);

        // opening the message
        Output('Starting message processing.');
        Reader.Open(memoInput.Text);

        // outputting the details
        Output('Message loaded successfully.');
        Output('General details: ');
        Output('  Destination: ' + Reader.Message.Destination);
        Output('  Issue instant: ' + Reader.Message.IssueInstant);
        Output('  ID: ' + Reader.Message.ID);
        Output('  Issuer: ' + Reader.Message.Issuer);
        Output('  In response to: ' + Reader.Message.InResponseTo);

        case Reader.Message.ContentType of
          cstyNone:
          begin
            Output('Message type: Unknown');
          end;
          cstyAssertionIDRequest:
          begin
            Output('Message type: AssertionIDRequest');
          end;
          cstySubjectQuery:
          begin
            Output('Message type: SubjectQuery');
            OutputSubjectQuery(Reader);
          end;
          cstyAuthnQuery:
          begin
            Output('Message type: AuthnQuery');
          end;
          cstyAttributeQuery:
          begin
            Output('Message type: AttributeQuery');
            OutputAttributeQuery(Reader);
          end;
          cstyAuthzDecisionQuery:
          begin
            Output('Message type: AuthzDecisionQuery');
          end;
          cstyAuthnRequest:
          begin
            Output('Message type: AuthnRequest');
            OutputAuthnRequest(Reader);
          end;
          cstyManageNameIDRequest:
          begin
            Output('Message type: ManageNameIDRequest');
          end;
          cstyLogoutRequest:
          begin
            Output('Message type: LogoutRequest');
            OutputLogoutRequest(Reader);
          end;
          cstyNameIDMappingRequest:
          begin
            Output('Message type: NameIDMappingRequest');
          end;
          cstyArtifactResolve:
          begin
            Output('Message type: ArtifactResolve');
          end;
          cstyResponse:
          begin
            Output('Message type: Response');
            OutputResponse(Reader);
          end;
        end;

        // global security props
        if Reader.Message.Signed or Reader.PinnedAssertion.Signed then
        begin
          Output('The processed message was signed by its author.');
          if Reader.SigningCertificate <> nil then
            Output('Signing certificate: ' + Reader.SigningCertificate.SubjectRDN)
          else
            Output('Signing certificate was not found');
          Output('Signature validation result: ' + IntToStr(integer(Reader.Message.SignatureValidationResult)));
          Output('Digest method: ' + Reader.Security.DigestMethod);
        end;

        if ((Reader.Message.ContentType = cstyAssertion) or (Reader.Message.ContentType = cstyResponse)) and
          (Reader.PinnedAssertion.AssertionType = csatEncryptedAssertion) then
        begin
          Output('The assertion was encrypted by its author.');
          Output('Encryption method: ' + Reader.Security.EncryptionMethod);
        end;
      finally
        FreeAndNil(Reader);
      end;

      MessageDlg('SAML message processed', mtConfirmation, [mbOK], 0);
    finally
      FreeAndNil(VerCertMgr);
      FreeAndNil(DecCertMgr);
    end;
  except
    on E : Exception do
      MessageDlg('Error happened: ' + E.Message, mtError, [mbOk], 0);
  end;
end;

procedure TFormSAMLReader.HandleReaderEncrypted(Sender: TObject; const IssuerRDN: String;
  const SerialNumber: TBytes; const SubjectKeyID: TBytes; NeedCredential: Boolean;
  var SkipThis: Boolean);
begin
  Output('The message is encrypted.');
  if NeedCredential then
    Output('The decryption certificate is not available. If you did provide a decryption certificate, make sure it matches the private key used by the message creator.')
  else
    Output('The decryption certificate provided matches.');
end;

procedure TFormSAMLReader.HandleReaderSignatureFound(Sender: TObject; Scope: Integer;
  const IssuerRDN: String; const SerialNumber: TBytes; const SubjectKeyID: TBytes;
  CertFound: Boolean; var Validate: Boolean);
begin
  Output('A signature was found in the entity being processed.');
  if Scope = sssAssertion then
    Output('The signature covers an assertion.')
  else if Scope = sssMessage then
    Output('The signature covers the whole message.')
  else if Scope = sssBinding then
    Output('The signature was made over the binding.')
  else
    Output('The signature covers something else - this doesn''t look right.');
  if CertFound then
  begin
    Output('The verification certificate is available, either by being included in the signature or provided by you.');
    Validate := true;
  end
  else
  begin
    Output('The verification certificate was not found - will be unable to validate the signature.');
    Validate := false;
  end;
end;

procedure TFormSAMLReader.HandleReaderSignatureValidated(Sender: TObject; Scope: Integer;
  const IssuerRDN: String; const SerialNumber: TBytes; const SubjectKeyID: TBytes;
  var ValidationResult: Integer);
var
  S: string;
begin
  case ValidationResult of
    svtValid : S := 'VALID';
    svtCorrupted : S := 'CORRUPTED';
    svtSignerNotFound : S := 'SIGNING CERTIFICATE NOT FOUND';
    svtFailure : S := 'FAILED TO VALIDATE';
    svtReferenceCorrupted : S := 'REFERENCE CORRUPTED';
  else
    S := 'UNKNOWN';
  end;

  Output('The signature has been validated. Validation result: ' + S);
end;

procedure TFormSAMLReader.OutputSubjectQuery(Reader : TsbxSAMLReader);
begin
  OutputSubjectConfirmations(Reader);
end;

procedure TFormSAMLReader.OutputAttributeQuery(Reader: TsbxSAMLReader);
begin
  OutputAttributes(Reader);
  OutputSubjectConfirmations(Reader);
end;

procedure TFormSAMLReader.OutputAuthnRequest(Reader: TsbxSAMLReader);
begin
  Output('  Assertion Consumer Service URL: ' + Reader.AuthnRequest.AssertionConsumerServiceURL);
  Output('  Protocol binding: ' + Reader.AuthnRequest.ProtocolBinding);
  Output('  Provider name: ' + Reader.AuthnRequest.ProviderName);
  Output('  NameID policy format: ' + Reader.AuthnRequest.NameIDPolicyFormat);
  Output('  Context class refs: ' + Reader.AuthnRequest.ContextClassRefs);
  OutputSubjectConfirmations(Reader);
  OutputConditions(Reader);
end;

procedure TFormSAMLReader.OutputLogoutRequest(Reader: TsbxSAMLReader);
begin
  Output('  NameID: ' + Reader.LogoutRequest.NameID);
  Output('  Not on or after: ' + Reader.LogoutRequest.NotOnOrAfter);
  Output('  Reason: ' + Reader.LogoutRequest.Reason);
  Output('  Session index(es): ' + Reader.LogoutRequest.SessionIndexes);
end;

procedure TFormSAMLReader.OutputResponse(Reader: TsbxSAMLReader);
var
  I : integer;
begin
  // outputting all assertions, one by one
  Output('  Assertions found: ' + IntToStr(Reader.AssertionCount));
  for I := 0 to Reader.AssertionCount - 1 do
  begin
    Output('    Assertion #' + IntToStr(I));
    Reader.PinAssertion(I); // selecting the assertion to process
    OutputAssertion(Reader);
  end;
end;

procedure TFormSAMLReader.OutputAssertion(Reader: TsbxSAMLReader);
begin
  // outputting currently selected ('pinned') assertion
  Output('  Issuer: ' + Reader.PinnedAssertion.Issuer);
  Output('  Assertion ID: ' + Reader.PinnedAssertion.ID);
  Output('  Subject: ' + Reader.PinnedAssertion.Subject);

  OutputStatements(Reader);
  OutputAttributes(Reader);
  OutputConditions(Reader);
  OutputSubjectConfirmations(Reader);
end;

procedure TFormSAMLReader.OutputSubjectConfirmations(Reader: TsbxSAMLReader);
var
  I : integer;
begin
  Output('  Subject confirmations:');
  for I := 0 to Reader.SubjectConfirmations.Count - 1 do
  begin
    Output('    Subject confirmation #' + IntToStr(I + 1) + ': ');
    Output('      Address: ' + Reader.SubjectConfirmations[I].Address);
    Output('      Data: ' + Reader.SubjectConfirmations[I].Data);
    Output('      Data type: ' + Reader.SubjectConfirmations[I].DataType);
    Output('      ID: ' + Reader.SubjectConfirmations[I].ID);
    Output('      Method: ' + Reader.SubjectConfirmations[I].Method);
    Output('      Not before: ' + Reader.SubjectConfirmations[I].NotBefore);
    Output('      Not after: ' + Reader.SubjectConfirmations[I].NotOnOrAfter);
  end;
end;

procedure TFormSAMLReader.OutputAttributes(Reader: TsbxSAMLReader);
var
  I : integer;
begin
  Output('  Attributes:');
  for I := 0 to Reader.Attributes.Count - 1 do
  begin
    Output('    Attribute #' + IntToStr(I + 1) + ': ');
    Output('      Name: ' + Reader.Attributes[I].Name);
    Output('      Name Format: ' + Reader.Attributes[I].NameFormat);
    Output('      Friendly Name: ' + Reader.Attributes[I].FriendlyName);
    Output('      Statement Index: ' + IntToStr(Reader.Attributes[I].StatementIndex));
    Output('      Value(s): ' + Reader.Attributes[I].Values);
  end;
end;

procedure TFormSAMLReader.OutputConditions(Reader: TsbxSAMLReader);
var
  I : integer;
  S : string;
begin
  Output('  Conditions:');
  for I := 0 to Reader.Conditions.Count - 1 do
  begin
    Output('    Condition #' + IntToStr(I + 1) + ': ');

    case Reader.Conditions[I].ConditionType of
      csctAudienceRestriction : S := 'Audience restriction';
      csctOneTimeUse : S := 'One-time use';
      csctProxyRestriction : S := 'Proxy restriction';
      csctNotBefore : S := 'Not before';
      csctNotOnOrAfter : S := 'Not after';
    end;

    Output('      Condition type: ' + S);
    Output('      Condition: ' + Reader.Conditions[I].Condition);
  end;
end;

procedure TFormSAMLReader.OutputStatements(Reader: TsbxSAMLReader);
var
  I : integer;
  S : string;
begin
  Output('  Statements:');
  for I := 0 to Reader.Statements.Count - 1 do
  begin
    Output('    Attribute #' + IntToStr(I + 1) + ': ');
    case Reader.Statements[I].StatementType of
      csastAuthn :
      begin
        Output('      Type: AuthnStatement');
        Output('      Context class ref: ' + Reader.Statements[I].AuthnContextClassRef);
        Output('      Instant: ' + Reader.Statements[I].AuthnInstant);
        Output('      Session index: ' + Reader.Statements[I].AuthnSessionIndex);
        Output('      Not on or after: ' + Reader.Statements[I].AuthnSessionNotOnOrAfter);
      end;
      csastAttribute :
      begin
        Output('      Type: AttributeStatement (see attributes below)');
      end;
      csastAuthzDecision :
      begin
        Output('      Type: AuthzDecisionStatement');
        Output('      Actions: ' + Reader.Statements[I].AuthzActions);
        Output('      Evidence: ' + Reader.Statements[I].AuthzDecisionEvidence);
        case Reader.Statements[I].AuthzDecision of
          csadnPermit : S := 'Permit';
          csadnDeny : S := 'Deny';
        else
          S := 'Indeterminate';
        end;
        Output('      Decision: ' + S);
      end;
      else
        Output('      Type: unknown');
    end;
  end;
end;

procedure TFormSAMLReader.Output(const S : string);
begin
  memoOutput.Lines.Add(S);
end;

end.

