<?php
/*
 * SecureBlackbox 2024 PHP Edition - Sample Project
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
 */
require_once('../include/secureblackbox_samlwriter.php');
require_once('../include/secureblackbox_const.php');
?>
<style>
table { width: 100% !important; }
td { white-space: nowrap; }
td input { width: 100%; }
td:last-child { width: 100%; }
</style>

<div width="90%">
  <form method=POST>
    <h2>SAML Writer Demo</h2>
    
    <h3>General Options</h3><br/><br/>
    <table>
      <tr><td>Issuer:</td><td><input type=text name=issuer value="http://saml.localservice.com/metadata/"></td></tr>
      <tr><td>Destination:</td><td><input type=text name=destination value="http://saml.remoteservice.com/sso"></td></tr>
      <tr><td>Service (e.g. ACS) URL:</td><td><input type=text name=serviceURL value="http://saml.localservice.com/acs"></td></tr>
      <tr>
        <td>SAML type:</td>
        <td>
          <select name="mestype">
            <option selected value="1">SP - AuthnRequest</option>
            <option value="2">SP - LogoutRequest</option>
            <option value="3">SP - AttributeQuery</option>
            <option value="4">SP - SubjectQuery</option>
            <option value="5">IdP - Assertion</option>
            <option value="6">IdP - Enveloped Assertion</option>
          </select>
        </td>
      </tr>
      <tr><td>Output:</td><td><input type=text name=output value=""></td></tr>
    </table>

    <br/><br/><h3>Security settings</h3><br/><br/>
    <b>
      <input type=checkbox id="isSign" name="isSign" />
      <label for=isSign>Sign the output</label>
    </b>
    <table>
      <tr><td>Signing certificate:</td><td><input type=text name=sCertFile value=""></td></tr>
      <tr><td>Password:</td><td><input type=password name=sCertPass value=""></td></tr>
      <tr><td>Hash Algorithm:</td><td>
        <div style="display: inline-block; margin: .25em 0;">
          <select name=hashAlgo id="hashAlgo">
            <option value="SHA1">SHA1</option>
            <option value="SHA224">SHA224</option>
            <option value="SHA256" selected>SHA256</option>
            <option value="SHA384">SHA384</option>
            <option value="SHA512">SHA512</option>
          </select>
        </div>
      </td></tr>
    </table>

    <b>
      <input type=checkbox id="isEnc" name="isEnc" />
      <label for=isEnc>Encrypt the output (assertions only):</label>
    </b>
    <table>
      <tr><td>Encryption certificate:</td><td><input type=text name=eCertFile value=""></td></tr>
    </table>

    <br/>
    <br/>

    <input type="submit" value="Create" />
  </form>
</div><br/>

<?php

function applySecurity($writer, $isSign, $sCertFile, $sCertPass, $hashAlgo, $isEnc, $eCertFile){
  $certmgr = new SecureBlackBox_CertificateManager();

  if($isSign){
    try {
      $certmgr->doImportFromFile($sCertFile, $sCertPass);
    } catch (exception $e) {
      echo "<h2>Failed to load certificate</h2><p>" . $e->getMessage() . "</p>";
      return;
    }

    $writer->setMessageSigned(TRUE);
    $writer->setSigningCertHandle($certmgr->getCertHandle());
    $writer->setSecurityDigestMethod($hashAlgo);
  }

  if($isEnc){
    try {
      $certmgr->doImportFromFile($eCertFile, "");
    } catch (exception $e) {
      echo "<h2>Failed to load certificate</h2><p>" . $e->getMessage() . "</p>";
      return;
    }

    $writer->setSecurityEncryptionMethod("AES128");
    $writer->setEncryptionCertHandle($certmgr->getCertHandle());
    $writer->setAssertionAssertionType(SAMLWRITER_ASSERTIONASSERTIONTYPE_ENCRYPTED_ASSERTION);
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $writer = new SecureBlackbox_SAMLWriter();

    try {
      // General options
      $mesType = $_REQUEST['mestype'];
      $issuer = $_REQUEST['issuer'];
      $destination = $_REQUEST['destination'];
      $serviceURL = $_REQUEST['serviceURL'];
      $output = $_REQUEST['output'];

      switch($mesType) {
        case "1": // CreateAuthnRequest
          // creating a message of AuthnRequest type
          $writer->doCreateNew(SAMLWRITER_MESSAGECONTENTTYPE_AUTHN_REQUEST);

          // main message properties
          $writer->setMessageID("my-message-id-123");
          $writer->setMessageIssueInstant(date("Y-m-d H:i:s"));
          $writer->setMessageDestination($destination);
          $writer->setMessageIssuer($writer->doFormatID($issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", ""));

          // adding a subject confirmation
          $writer->setMessageSubject($writer->getMessageIssuer());
          $writer->doAddSubjectConfirmation("scmethod", "http://scaddress.com", "screcipient", "", date("Y-m-d H:i:s", strtotime('-1 years')), date("Y-m-d H:i:s", strtotime('+1 years')), "", "sctype", "scdata");

          // adding a couple of conditions
          $writer->doAddCondition(SAMLWRITER_CONDITIONCONDITIONTYPE_AUDIENCE_RESTRICTION, "PSCU:saml20:dev");
          $writer->doAddCondition(SAMLWRITER_CONDITIONCONDITIONTYPE_NOT_BEFORE, date("Y-m-d H:i:s"));

          // setting up authnrequest parameters
          $writer->setAuthnRequestAssertionConsumerServiceIndex(0);
          $writer->setAuthnRequestAssertionConsumerServiceURL($serviceURL);
          $writer->setAuthnRequestProtocolBinding("urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST");
          $writer->setAuthnRequestProviderName("My Application");
          $writer->setAuthnRequestNameIDPolicyFormat("urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress");
          $writer->setAuthnRequestNameIDPolicyAllowCreate(TRUE);
          $writer->setAuthnRequestContextComparison(SAMLWRITER_AUTHNREQUESTCONTEXTCOMPARISON_EXACT);
          $writer->setAuthnRequestContextClassRefs("urn:oasis:names:tc:SAML:2.0:ac:classes:PasswordProtectedTransport");
          $writer->setAuthnRequestContextRefType(SAMLWRITER_AUTHNREQUESTCONTEXTREFTYPE_CLASS);

          // applying security
          applySecurity($writer, !empty($_REQUEST['isSign']), $_REQUEST['sCertFile'], $_REQUEST['sCertPass'], $_REQUEST['hashAlgo'], FALSE, "");

          // Saving the output
          $writer->doSaveFile($output);
          break;
        case "2": // CreateLogoutRequest
          // creating a message of LogoutRequest type
          $writer->doCreateNew(SAMLWRITER_MESSAGECONTENTTYPE_LOGOUT_REQUEST);

          // main message properties
          $writer->setMessageID("my-message-id-123");
          $writer->setMessageIssueInstant(date("Y-m-d H:i:s"));
          $writer->setMessageDestination($destination);
          $writer->setMessageIssuer($writer->doFormatID($issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", ""));

          // setting up logoutrequest parameters
          $writer->setLogoutRequestNameID($writer->doFormatID("id-abcdefghijkl", "", "urn:oasis:names:tc:SAML:2.0:nameid-format:transient", "", "", ""));
          $writer->setLogoutRequestNotOnOrAfter(date("Y-m-d H:i:s"));
          $writer->setLogoutRequestReason("Requested by user");
          $writer->setLogoutRequestSessionIndexes("id-01234567890");

          // applying security
          applySecurity($writer, !empty($_REQUEST['isSign']), $_REQUEST['sCertFile'], $_REQUEST['sCertPass'], $_REQUEST['hashAlgo'], FALSE, "");

          // Saving the output
          $writer->doSaveFile($output);
          break;
        case "3": // CreateAttributeQuery
          // creating a message of AttributeQuery type
          $writer->doCreateNew(SAMLWRITER_MESSAGECONTENTTYPE_ATTRIBUTE_QUERY);

          // main message properties
          $writer->setMessageID("my-message-id-123");
          $writer->setMessageIssueInstant(date("Y-m-d H:i:s"));
          $writer->setMessageDestination($destination);
          $writer->setMessageIssuer($writer->doFormatID($issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", ""));

          // setting up attributequery parameters: a couple of attributes we want
          $writer->doAddAttribute("clientID", "JohnDoe123", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic", "xs:string", 0);
          $writer->doAddAttribute("clientCategorty", "Business", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic", "xs:string", 0);

          // ... and a subject confirmation
          $writer->setMessageSubject($writer->getMessageIssuer());
          $writer->doAddSubjectConfirmation("scmethod", "http://scaddress.com", "screcipient", "", date("Y-m-d H:i:s", strtotime('-1 years')), date("Y-m-d H:i:s", strtotime('+1 years')), "", "sctype", "scdata");

          // applying security
          applySecurity($writer, !empty($_REQUEST['isSign']), $_REQUEST['sCertFile'], $_REQUEST['sCertPass'], $_REQUEST['hashAlgo'], FALSE, "");

          // Saving the output
          $writer->doSaveFile($output);
          break;
        case "4": // CreateSubjectQuery
          // creating a message of SubjectQuery type
          $writer->doCreateNew(SAMLWRITER_MESSAGECONTENTTYPE_SUBJECT_QUERY);

          // main message properties
          $writer->setMessageID("my-message-id-123");
          $writer->setMessageIssueInstant(date("Y-m-d H:i:s"));
          $writer->setMessageDestination($destination);
          $writer->setMessageIssuer($writer->doFormatID($issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", ""));

          // setting up subjectquery parameters: a couple of subject confirmations
          $writer->setMessageSubject($writer->getMessageIssuer());
          $writer->doAddSubjectConfirmation("scmethod1", "http://scaddress.com", "screcipient", "", date("Y-m-d H:i:s", strtotime('-1 years')), date("Y-m-d H:i:s", strtotime('+1 years')), "", "sctype", "scdata");
          $writer->doAddSubjectConfirmation("scmethod2", "Sandford, Gloucestershire", "screcipient", "", date("Y-m-d H:i:s", strtotime('-1 years')), date("Y-m-d H:i:s", strtotime('+3 years')), "", "sctype", "scdata");

          // applying security
          applySecurity($writer, !empty($_REQUEST['isSign']), $_REQUEST['sCertFile'], $_REQUEST['sCertPass'], $_REQUEST['hashAlgo'], FALSE, "");

          // Saving the output
          $writer->doSaveFile($output);
          break;
        case "5": // CreateAssertion
          // creating a message of Assertion type
          $writer->doCreateNew(SAMLWRITER_MESSAGECONTENTTYPE_ASSERTION);

          // main message properties
          $writer->setMessageID("my-message-id-123");
          $writer->setMessageIssueInstant(date("Y-m-d H:i:s"));
          $writer->setMessageDestination($destination);
          $writer->setMessageIssuer($writer->doFormatID($issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", ""));

          // a message may contain multiple complex assertions, each of which
          // should be enveloped into doBeginAssertion/doCompleteAssertion calls.
          $writer->doBeginAssertion();
          $writer->setAssertionIssuer($writer->getMessageIssuer()); // keeping it simple
          $writer->setAssertionAssertionType(SAMLWRITER_ASSERTIONASSERTIONTYPE_ASSERTION);

          // An assertion may contain multiple attributes within multiple statements.
          // If we add an attribute without adding a statement first, a new statement
          // will be created automatically, and the attribute added to it.
          // For example, the below two attributes go to the first statement,
          // which is created automatically:
          $writer->doAddAttribute("clientID", "JohnDoe123", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic", "xs:string", 0);
          $writer->doAddAttribute("clientCategorty", "Business", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic", "xs:string", 0);

          // Let's add another statement
          $writer->doAddAttributeStatement(); // returns the index, which is going to be 1

          // Adding one attribute with two values - these are going to be merged
          // because their names are identical.
          $writer->doAddAttribute("eduPersonAffiliation", "users", "", "", 1);
          $writer->doAddAttribute("eduPersonAffiliation", "examplerole1", "", "", 1);

          // adding an authentication statement
          $writer->doAddAuthnStatement(date("Y-m-d H:i:s"), "id-1234567890", date("Y-m-d H:i:s", strtotime('+1 years')), "urn:oasis:names:tc:SAML:2.0:ac:classes:Password");

          // adding an authorization statement
          $writer->doAddAuthzDecisionStatement(SAMLWRITER_STATEMENTAUTHZDECISION_PERMIT, "Evidence", "Resource", "namespace=ns1;value=value1");

          // adding assertion conditions (audience and time scope)
          $writer->doAddCondition(SAMLWRITER_CONDITIONCONDITIONTYPE_AUDIENCE_RESTRICTION, "PSCU:saml20:dev");
          $writer->doAddCondition(SAMLWRITER_CONDITIONCONDITIONTYPE_NOT_ON_OR_AFTER, date("Y-m-d H:i:s", strtotime('+1 years')));

          // setting custom assertion ID (optional)
          $writer->setAssertionID("unique-id-123456");

          // adding subject confirmations
          $writer->setAssertionSubject($writer->doFormatID('Subject', 'Subject', "", "", "", ""));
          $writer->doAddSubjectConfirmation("scmethod1", "http://scaddress.com", "screcipient", "", date("Y-m-d H:i:s", strtotime('-1 years')), date("Y-m-d H:i:s", strtotime('+1 years')), "", "sctype", "scdata");

          // applying security: as it applies to the assertion, it should precede
          // the doCompleteAssertion() call
          applySecurity($writer, !empty($_REQUEST['isSign']), $_REQUEST['sCertFile'], $_REQUEST['sCertPass'], $_REQUEST['hashAlgo'], !empty($_REQUEST['isEnc']), $_REQUEST['eCertFile']);

          // adding ("committing") the formed assertion to the SAML message
          $writer->doCompleteAssertion();

          // Saving the output
          $writer->doSaveFile($output);
          break;
        case "6": // CreateEnvelopedAssertion
          // creating a message of Response type
          $writer->doCreateNew(SAMLWRITER_MESSAGECONTENTTYPE_RESPONSE);

          // main message properties
          $writer->setMessageID("my-message-id-123");
          $writer->setMessageIssueInstant(date("Y-m-d H:i:s"));
          $writer->setMessageDestination($destination);
          $writer->setMessageIssuer($writer->doFormatID($issuer, "Issuer", "urn:oasis:names:tc:SAML:2.0:nameid-format:entity", "", "", ""));

          // a message may contain multiple complex assertions, each of which
          // should be enveloped into doBeginAssertion/doCompleteAssertion calls.
          $writer->doBeginAssertion();
          $writer->setAssertionIssuer($writer->getMessageIssuer());  // keeping it simple
          $writer->setAssertionAssertionType(SAMLWRITER_ASSERTIONASSERTIONTYPE_ASSERTION);

          // An assertion may contain multiple attributes within multiple statements.
          // If we add an attribute without adding a statement first, a new statement
          // will be created automatically, and the attribute added to it.
          // For example, the below two attributes go to the first statement,
          // which is created automatically:
          $writer->doAddAttribute("clientID", "JohnDoe123", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic", "xs:string", 0);
          $writer->doAddAttribute("clientCategorty", "Business", "urn:oasis:names:tc:SAML:2.0:attrname-format:basic", "xs:string", 0);

          // adding an authentication statement
          $writer->doAddAuthnStatement(date("Y-m-d H:i:s"), "id-1234567890", date("Y-m-d H:i:s", strtotime('+1 years')), "urn:oasis:names:tc:SAML:2.0:ac:classes:Password");

          // adding assertion conditions (audience and time scope)
          $writer->doAddCondition(SAMLWRITER_CONDITIONCONDITIONTYPE_AUDIENCE_RESTRICTION, "PSCU:saml20:dev");
          $writer->doAddCondition(SAMLWRITER_CONDITIONCONDITIONTYPE_NOT_ON_OR_AFTER, date("Y-m-d H:i:s", strtotime('+1 years')));

          // setting custom assertion ID (optional)
          $writer->setAssertionID("unique-id-123456");

          // adding subject confirmations
          $writer->setAssertionSubject($writer->doFormatID("Subject", "Subject", "", "", "", ""));
          $writer->doAddSubjectConfirmation("scmethod1", "http://scaddress.com", "screcipient", "", date("Y-m-d H:i:s", strtotime('-1 years')), date("Y-m-d H:i:s", strtotime('+1 years')), "", "sctype", "scdata");

          // applying security: as it applies to the assertion, it should precede
          // the doCompleteAssertion() call
          applySecurity($writer, !empty($_REQUEST['isSign']), $_REQUEST['sCertFile'], $_REQUEST['sCertPass'], $_REQUEST['hashAlgo'], !empty($_REQUEST['isEnc']), $_REQUEST['eCertFile']);

          // adding ("committing") the formed assertion to the SAML message
          $writer->doCompleteAssertion();

          // Saving the output
          $writer->doSaveFile($output);
          break;
        default:
          echo "<h2>Error: Unknown choice of the output message!</h2>";
          return;
      }

      echo "<h2>SAML message successfully create</h2>";
    }
    catch (exception $e) {
      echo "<h2>Creating Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>