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
unit samlwriterf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  sbxcertificatemanager, sbxutils, sbxsamlwriter, sbxtypes, sbxconstants;

type
  TFormSAMLWriter = class(TForm)
    gbSecurity: TGroupBox;
    lblEncCert: TLabel;
    editEncCert: TEdit;
    btnBrowseEncCert: TButton;
    lblSigningCert: TLabel;
    editSigningCert: TEdit;
    btnBrowseSigningCert: TButton;
    lblCertPassword: TLabel;
    editPassword: TEdit;
    lblHashAlg: TLabel;
    cbHashAlg: TComboBox;
    cbSign: TCheckBox;
    cbEncrypt: TCheckBox;
    gbGeneral: TGroupBox;
    lblIssuer: TLabel;
    editIssuer: TEdit;
    lblDestination: TLabel;
    lblService: TLabel;
    editDestination: TEdit;
    editService: TEdit;
    gbGenerator: TGroupBox;
    lblIWant: TLabel;
    cbOutputType: TComboBox;
    btnGo: TButton;
    memoOutput: TMemo;
    dlgSigningCert: TOpenDialog;
    dlgEncCert: TOpenDialog;
    procedure btnBrowseSigningCertClick(Sender: TObject);
    procedure btnBrowseEncCertClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Utils : TsbxUtils;
    function CreateAuthnRequest(SigCertMgr: TsbxCertificateManager): string;
    function CreateLogoutRequest(SigCertMgr : TsbxCertificateManager): string;
    function CreateAttributeQuery(SigCertMgr : TsbxCertificateManager): string;
    function CreateSubjectQuery(SigCertMgr : TsbxCertificateManager): string;
    function CreateAssertion(SigCertMgr, EncCertMgr : TsbxCertificateManager): string;
    function CreateEnvelopedAssertion(SigCertMgr, EncCertMgr : TsbxCertificateManager): string;

    procedure ApplySecurity(Writer: TsbxSAMLWriter; SigCertMgr, EncCertMgr : TsbxCertificateManager);
  public
    { Public declarations }
  end;

var
  FormSAMLWriter: TFormSAMLWriter;

implementation

{$R *.dfm}

procedure TFormSAMLWriter.btnBrowseEncCertClick(Sender: TObject);
begin
  if dlgEncCert.Execute then
    editEncCert.Text := dlgEncCert.FileName;
end;

procedure TFormSAMLWriter.btnBrowseSigningCertClick(Sender: TObject);
begin
  if dlgSigningCert.Execute then
    editSigningCert.Text := dlgSigningCert.FileName;
end;

procedure TFormSAMLWriter.btnGoClick(Sender: TObject);
var
  SigCertMgr, EncCertMgr : TsbxCertificateManager;
  Saml : string;
begin
  SigCertMgr := nil;
  EncCertMgr := nil;

  try
    try
      // loading the signing certificate
      if cbSign.Checked then
      begin
        SigCertMgr := TsbxCertificateManager.Create(nil);
        SigCertMgr.ImportFromFile(editSigningCert.Text, editPassword.Text);
      end;

      // loading the encryption certificate
      if cbEncrypt.Checked then
      begin
        EncCertMgr := TsbxCertificateManager.Create(nil);
        EncCertMgr.ImportFromFile(editEncCert.Text, '');
      end;

      // creating a SAML message of the chosen type
      case cbOutputType.ItemIndex of
        0 :
          Saml := CreateAuthnRequest(SigCertMgr);
        1 :
          Saml := CreateLogoutRequest(SigCertMgr);
        2 :
          Saml := CreateAttributeQuery(SigCertMgr);
        3 :
          Saml := CreateSubjectQuery(SigCertMgr);
        4 :
          Saml := CreateAssertion(SigCertMgr, EncCertMgr);
        5 :
          Saml := CreateEnvelopedAssertion(SigCertMgr, EncCertMgr);
      else
        raise Exception.Create('Unknown choice of the output message!');
      end;

      memoOutput.Lines.Text := Saml;
      MessageDlg('SAML message created', mtConfirmation, [mbOK], 0);
    finally
      FreeAndNil(SigCertMgr);
      FreeAndNil(EncCertMgr);
    end;
  except
    on E : Exception do
      MessageDlg('Error happened: ' + E.Message, mtError, [mbOk], 0);
  end;
end;

function TFormSAMLWriter.CreateAuthnRequest(SigCertMgr : TsbxCertificateManager): string;
var
  Writer : TsbxSAMLWriter;
begin
  Writer := TsbxSAMLWriter.Create(nil);
  try
    // creating a message of AuthnRequest type
    Writer.CreateNew(integer(cstyAuthnRequest));

    // main message properties
    Writer.Message.ID := 'my-message-id-123';
    Writer.Message.IssueInstant := Utils.DateToString(Now);
    Writer.Message.Destination := editDestination.Text;
    Writer.Message.Issuer := Writer.FormatID(editIssuer.Text, 'Issuer', 'urn:oasis:names:tc:SAML:2.0:nameid-format:entity', '', '', '');

    // adding a subject confirmation
    Writer.Message.Subject := Writer.Message.Issuer;
    Writer.AddSubjectConfirmation('scmethod', 'http://scaddress.com', 'screcipient', '',
      Utils.DateToString(Now - 1), Utils.DateToString(Now + 1), '', 'sctype', 'scdata');

    // adding a couple of conditions
    Writer.AddCondition(integer(csctAudienceRestriction), 'PSCU:saml20:dev');
    Writer.AddCondition(integer(csctNotBefore), Utils.DateToString(Now));

    // setting up authnrequest parameters
    Writer.AuthnRequest.AssertionConsumerServiceIndex := 0;
    Writer.AuthnRequest.AssertionConsumerServiceURL := editService.Text;
    Writer.AuthnRequest.ProtocolBinding := 'urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST';
    Writer.AuthnRequest.ProviderName := 'My Application';
    Writer.AuthnRequest.NameIDPolicyFormat := 'urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress';
    Writer.AuthnRequest.NameIDPolicyAllowCreate := true;
    Writer.AuthnRequest.ContextComparison := cacctExact;
    Writer.AuthnRequest.ContextClassRefs := 'urn:oasis:names:tc:SAML:2.0:ac:classes:PasswordProtectedTransport';
    Writer.AuthnRequest.ContextRefType := cacrtClass;

    // applying security
    ApplySecurity(Writer, SigCertMgr, nil);

    // Saving the output
    Result := Writer.save();
  finally
    FreeAndNil(Writer);
  end;
end;

function TFormSAMLWriter.CreateLogoutRequest(SigCertMgr : TsbxCertificateManager): string;
var
  Writer : TsbxSAMLWriter;
begin
  Writer := TsbxSAMLWriter.Create(nil);
  try
    // creating a message of LogoutRequest type
    Writer.CreateNew(integer(cstyLogoutRequest));

    // main message properties
    Writer.Message.ID := 'my-message-id-123';
    Writer.Message.IssueInstant := Utils.DateToString(Now);
    Writer.Message.Destination := editDestination.Text;
    Writer.Message.Issuer := Writer.FormatID(editIssuer.Text, 'Issuer', 'urn:oasis:names:tc:SAML:2.0:nameid-format:entity', '', '', '');

    // setting up logoutrequest parameters
    Writer.LogoutRequest.NameID := Writer.FormatID('id-abcdefghijkl', '', 'urn:oasis:names:tc:SAML:2.0:nameid-format:transient', '', '', '');
    Writer.LogoutRequest.NotOnOrAfter := Utils.DateToString(Now + 1);
    Writer.LogoutRequest.Reason := 'Requested by user';
    Writer.LogoutRequest.SessionIndexes := 'id-01234567890';

    // applying security
    ApplySecurity(Writer, SigCertMgr, nil);

    // Saving the output
    Result := Writer.save();
  finally
    FreeAndNil(Writer);
  end;
end;

function TFormSAMLWriter.CreateAttributeQuery(SigCertMgr : TsbxCertificateManager): string;
var
  Writer : TsbxSAMLWriter;
begin
  Writer := TsbxSAMLWriter.Create(nil);
  try
    // creating a message of AttributeQuery type
    Writer.CreateNew(integer(cstyAttributeQuery));

    // main message properties
    Writer.Message.ID := 'my-message-id-123';
    Writer.Message.IssueInstant := Utils.DateToString(Now);
    Writer.Message.Destination := editDestination.Text;
    Writer.Message.Issuer := Writer.FormatID(editIssuer.Text, 'Issuer', 'urn:oasis:names:tc:SAML:2.0:nameid-format:entity', '', '', '');

    // setting up attributequery parameters: a couple of attributes we want
    Writer.AddAttribute('clientID', 'JohnDoe123', 'urn:oasis:names:tc:SAML:2.0:attrname-format:basic',
      'xs:string', 0);
    Writer.AddAttribute('clientCategorty', 'Business', 'urn:oasis:names:tc:SAML:2.0:attrname-format:basic',
      'xs:string', 0);

    // ... and a subject confirmation
    Writer.Message.Subject := Writer.Message.Issuer;
    Writer.AddSubjectConfirmation('scmethod', 'http://scaddress.com', 'screcipient', '',
      Utils.DateToString(Now - 1), Utils.DateToString(Now + 1), '', 'sctype', 'scdata');

    // applying security
    ApplySecurity(Writer, SigCertMgr, nil);

    // Saving the output
    Result := Writer.save();
  finally
    FreeAndNil(Writer);
  end;
end;

function TFormSAMLWriter.CreateSubjectQuery(SigCertMgr : TsbxCertificateManager): string;
var
  Writer : TsbxSAMLWriter;
begin
  Writer := TsbxSAMLWriter.Create(nil);
  try
    // creating a message of SubjectQuery type
    Writer.CreateNew(integer(cstySubjectQuery));

    // main message properties
    Writer.Message.ID := 'my-message-id-123';
    Writer.Message.IssueInstant := Utils.DateToString(Now);
    Writer.Message.Destination := editDestination.Text;
    Writer.Message.Issuer := Writer.FormatID(editIssuer.Text, 'Issuer', 'urn:oasis:names:tc:SAML:2.0:nameid-format:entity', '', '', '');

    // setting up subjectquery parameters: a couple of subject confirmations
    Writer.Message.Subject := Writer.Message.Issuer;
    Writer.AddSubjectConfirmation('scmethod1', 'http://scaddress.com', 'screcipient', '',
      Utils.DateToString(Now - 1), Utils.DateToString(Now + 1), '', 'sctype', 'scdata');
    Writer.AddSubjectConfirmation('scmethod2', 'Sandford, Gloucestershire', 'screcipient', '',
      Utils.DateToString(Now - 1), Utils.DateToString(Now + 3), '', 'sctype', 'scdata');

    // applying security
    ApplySecurity(Writer, SigCertMgr, nil);

    // Saving the output
    Result := Writer.save();
  finally
    FreeAndNil(Writer);
  end;
end;

function TFormSAMLWriter.CreateAssertion(SigCertMgr, EncCertMgr : TsbxCertificateManager): string;
var
  Writer : TsbxSAMLWriter;
begin
  Writer := TsbxSAMLWriter.Create(nil);
  try
    // creating a message of Assertion type
    Writer.CreateNew(integer(cstyAssertion));

    // main message properties
    Writer.Message.ID := 'my-message-id-123';
    Writer.Message.IssueInstant := Utils.DateToString(Now);
    Writer.Message.Destination := editDestination.Text;
    Writer.Message.Issuer := Writer.FormatID(editIssuer.Text, 'Issuer', 'urn:oasis:names:tc:SAML:2.0:nameid-format:entity', '', '', '');

    // a message may contain multiple complex assertions, each of which
    // should be enveloped into BeginAssertion/CompleteAssertion calls.
    Writer.BeginAssertion();
    Writer.Assertion.Issuer := Writer.Message.Issuer; // keeping it simple
    Writer.Assertion.AssertionType := csatAssertion;

    // An assertion may contain multiple attributes within multiple statements.
    // If we add an attribute without adding a statement first, a new statement
    // will be created automatically, and the attribute added to it.
    // For example, the below two attributes go to the first statement,
    // which is created automatically:
    Writer.AddAttribute('clientID', 'JohnDoe123', 'urn:oasis:names:tc:SAML:2.0:attrname-format:basic',
      'xs:string', 0);
    Writer.AddAttribute('clientCategorty', 'Business', 'urn:oasis:names:tc:SAML:2.0:attrname-format:basic',
      'xs:string', 0);

    // Let's add another statement
    Writer.AddAttributeStatement(); // returns the index, which is going to be 1

    // Adding one attribute with two values - these are going to be merged
    // because their names are identical.
    Writer.AddAttribute('eduPersonAffiliation', 'users', '', '', 1);
    Writer.AddAttribute('eduPersonAffiliation', 'examplerole1', '', '', 1);

    // adding an authentication statement
    Writer.AddAuthnStatement(Utils.DateToString(Now), 'id-1234567890',
      Utils.DateToString(Now + 1), 'urn:oasis:names:tc:SAML:2.0:ac:classes:Password');

    // adding an authorization statement
    Writer.AddAuthzDecisionStatement(integer(TsbxSAMLDecisions.csadnPermit),
      'Evidence', 'Resource', 'namespace=ns1;value=value1');

    // adding assertion conditions (audience and time scope)
    Writer.AddCondition(integer(csctAudienceRestriction), 'PSCU:saml20:dev');
    Writer.AddCondition(integer(csctNotOnOrAfter), Utils.DateToString(Now + 1));

    // setting custom assertion ID (optional)
    Writer.Assertion.ID := 'unique-id-123456';

    // adding subject confirmations
    Writer.Assertion.Subject := Writer.FormatID('Subject', 'Subject', '', '', '', '');
    Writer.AddSubjectConfirmation('scmethod1', 'http://scaddress.com', 'screcipient', '',
      Utils.DateToString(Now - 1), Utils.DateToString(Now + 1), '', 'sctype', 'scdata');

    // applying security: as it applies to the assertion, it should precede
    // the CompleteAssertion() call
    ApplySecurity(Writer, SigCertMgr, EncCertMgr);

    // adding ("committing") the formed assertion to the SAML message
    Writer.CompleteAssertion();

    // Saving the output
    Result := Writer.save();
  finally
    FreeAndNil(Writer);
  end;
end;

function TFormSAMLWriter.CreateEnvelopedAssertion(SigCertMgr, EncCertMgr : TsbxCertificateManager): string;
var
  Writer : TsbxSAMLWriter;
begin
  // This method creates a SAML entity of 'Response' type: an envelope that
  // incorporates an assertion. This is a typical body sent from the IdP
  // to the SP upon successful user authentication.

  Writer := TsbxSAMLWriter.Create(nil);
  try
    // creating a message of Response type
    Writer.CreateNew(integer(cstyResponse));

    // main message properties
    Writer.Message.ID := 'my-message-id-123';
    Writer.Message.IssueInstant := Utils.DateToString(Now);
    Writer.Message.Destination := editDestination.Text;
    Writer.Message.Issuer := Writer.FormatID(editIssuer.Text, 'Issuer', 'urn:oasis:names:tc:SAML:2.0:nameid-format:entity', '', '', '');

    // a message may contain multiple complex assertions, each of which
    // should be enveloped into BeginAssertion/CompleteAssertion calls.
    Writer.BeginAssertion();
    Writer.Assertion.Issuer := Writer.Message.Issuer; // keeping it simple
    Writer.Assertion.AssertionType := csatAssertion;

    // An assertion may contain multiple attributes within multiple statements.
    // If we add an attribute without adding a statement first, a new statement
    // will be created automatically, and the attribute added to it.
    // For example, the below two attributes go to the first statement,
    // which is created automatically:
    Writer.AddAttribute('clientID', 'JohnDoe123', 'urn:oasis:names:tc:SAML:2.0:attrname-format:basic',
      'xs:string', 0);
    Writer.AddAttribute('clientCategorty', 'Business', 'urn:oasis:names:tc:SAML:2.0:attrname-format:basic',
      'xs:string', 0);

    // adding an authentication statement
    Writer.AddAuthnStatement(Utils.DateToString(Now), 'id-1234567890',
      Utils.DateToString(Now + 1), 'urn:oasis:names:tc:SAML:2.0:ac:classes:Password');

    // adding assertion conditions (audience and time scope)
    Writer.AddCondition(integer(csctAudienceRestriction), 'PSCU:saml20:dev');
    Writer.AddCondition(integer(csctNotOnOrAfter), Utils.DateToString(Now + 1));

    // setting custom assertion ID (optional)
    Writer.Assertion.ID := 'unique-id-123456';

    // adding subject confirmations
    Writer.Assertion.Subject := Writer.FormatID('Subject', 'Subject', '', '', '', '');
    Writer.AddSubjectConfirmation('scmethod1', 'http://scaddress.com', 'screcipient', '',
      Utils.DateToString(Now - 1), Utils.DateToString(Now + 1), '', 'sctype', 'scdata');

    // applying security: as it applies to the assertion, it should precede
    // the CompleteAssertion() call
    ApplySecurity(Writer, SigCertMgr, EncCertMgr);

    // adding ("committing") the formed assertion to the SAML message
    Writer.CompleteAssertion();

    // Saving the output
    Result := Writer.save();
  finally
    FreeAndNil(Writer);
  end;
end;

procedure TFormSAMLWriter.ApplySecurity(Writer: TsbxSAMLWriter; SigCertMgr, EncCertMgr : TsbxCertificateManager);
begin
  if SigCertMgr <> nil then
  begin
    // This setting enforces signature upon the topmost element: a Response
    // or an Assertion (as per Writer.Message.ContentType).
    // If you would rather sign an Assertion residing within the enveloping
    // Response, set Writer.Assertion.Signed to true instead.
    Writer.Message.Signed := true;
    Writer.SigningCertificate := SigCertMgr.Certificate;
    Writer.Security.DigestMethod := cbHashAlg.Text;
  end;

  if EncCertMgr <> nil then
  begin
    Writer.EncryptionCertificate := EncCertMgr.Certificate;
    Writer.Security.EncryptionMethod := 'AES128';
    Writer.Assertion.AssertionType := csatEncryptedAssertion;
  end;
end;

procedure TFormSAMLWriter.FormCreate(Sender: TObject);
begin
  Utils := TsbxUtils.Create(nil);
end;

procedure TFormSAMLWriter.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Utils);
end;

end.

