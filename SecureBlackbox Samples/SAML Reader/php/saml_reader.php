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
require_once('../include/secureblackbox_samlreader.php');
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
    <h2>SAML Reader Demo</h2>
    
    <h3>General Options</h3><br/><br/>
    <table>
      <tr><td>Input:</td><td><input type=text name=input value=""></td></tr>
    </table>

    <br/><br/><h3>Security settings</h3><br/><br/>
    <table>
      <tr><td>Verifying certificate:</td><td><input type=text name=vCertFile value=""></td></tr>
      <tr><td>Decryption certificate:</td><td><input type=text name=dCertFile value=""></td></tr>
      <tr><td>Password:</td><td><input type=password name=dCertPass value=""></td></tr>
    </table>

    <br/>
    <br/>

    <input type="submit" value="Go" />
  </form>
</div><br/>

<?php

function Output($mes){
  echo $mes . "<br/>";
}

class MySAMLReader extends SecureBlackbox_SAMLReader
{
  function fireEncrypted($param) {
    Output("The message is encrypted.");

    if ($param['needcredential']) {
      Output("The decryption certificate is not available. If you did provide a decryption certificate, make sure it matches the private key used by the message creator.");
    }
    else {
      Output("The decryption certificate provided matches.");
    }

    return $param;
  }
  
  function fireSignatureFound($param) {
    Output("A signature was found in the entity being processed.");

    if ($param['scope'] == sssAssertion) {
      Output("The signature covers an assertion.");
    }
    else if ($param['scope'] == sssMessage) {
      Output("The signature covers the whole message.");
    }
    else if ($param['scope'] == sssBinding) {
      Output("The signature was made over the binding.");
    }
    else {
      Output("The signature covers something else - this doesn''t look right.");
    }

    if ($param['certfound']) {
      Output("The verification certificate is available, either by being included in the signature or provided by you.");
      $param['validate'] = TRUE;
    }
    else {
      Output("The verification certificate was not found - will be unable to validate the signature.");
      $param['validate'] = FALSE;
    }

    return $param;
  }
  
  function fireSignatureValidated($param) {
    $mes = "UNKNOWN";

    switch($param['validationresult']) {
      case SAMLREADER_MESSAGESIGNATUREVALIDATIONRESULT_VALID:
        $mes = "VALID";
        break;
      case SAMLREADER_MESSAGESIGNATUREVALIDATIONRESULT_CORRUPTED:
        $mes = "CORRUPTED";
        break;
      case SAMLREADER_MESSAGESIGNATUREVALIDATIONRESULT_SIGNER_NOT_FOUND:
        $mes = "SIGNING CERTIFICATE NOT FOUND";
        break;
      case SAMLREADER_MESSAGESIGNATUREVALIDATIONRESULT_FAILURE:
        $mes = "FAILED TO VALIDATE";
        break;
      case SAMLREADER_MESSAGESIGNATUREVALIDATIONRESULT_REFERENCE_CORRUPTED:
        $mes = "REFERENCE CORRUPTED";
        break;
      default:
        break;
    }

    Output("The signature has been validated. Validation result: " . $mes);

    return $param;
  }
}

function OutputSubjectQuery($reader) {
  OutputSubjectConfirmations($reader);
}

function OutputAttributeQuery($reader) {
  OutputAttributes($reader);
  OutputSubjectConfirmations($reader);
}

function OutputAuthnRequest($reader) {
  Output("&emsp;Assertion Consumer Service URL: " . $reader->getAuthnRequestAssertionConsumerServiceURL());
  Output("&emsp;Protocol binding: " . $reader->getAuthnRequestProtocolBinding());
  Output("&emsp;Provider name: " . $reader->getAuthnRequestProviderName());
  Output("&emsp;NameID policy format: " . $reader->getAuthnRequestNameIDPolicyFormat());
  Output("&emsp;Context class refs: " . $reader->getAuthnRequestContextClassRefs());
  OutputSubjectConfirmations($reader);
  OutputConditions($reader);
}

function OutputLogoutRequest($reader) {
  Output("&emsp;NameID: " . $reader->getLogoutRequestNameID());
  Output("&emsp;Not on or after: " . $reader->getLogoutRequestNotOnOrAfter());
  Output("&emsp;Reason: " . $reader->getLogoutRequestReason());
  Output("&emsp;Session index(es): " . $reader->getLogoutRequestSessionIndexes());
}

function OutputResponse($reader) {
  // outputting all assertions, one by one
  Output("&emsp;Assertions found: " . $reader->getAssertionCount());
  for ($i = 0; $i < $reader->getAssertionCount(); $i++) {
    Output("&emsp;&emsp;Assertion #" . ($i + 1));
    $reader->doPinAssertion($i); // selecting the assertion to process
    OutputAssertion($reader);
  }
}

function OutputAssertion($reader) {
  // outputting currently selected ('pinned') assertion
  Output("&emsp;Issuer: " . $reader->getPinnedAssertionIssuer());
  Output("&emsp;Assertion ID: " . $reader->getPinnedAssertionID());
  Output("&emsp;Subject: " . $reader->getPinnedAssertionSubject());

  OutputStatements($reader);
  OutputAttributes($reader);
  OutputConditions($reader);
  OutputSubjectConfirmations($reader);
}

function OutputSubjectConfirmations($reader) {
  Output("&emsp;Subject confirmations:");
  for ($i = 0; $i < $reader->getSubjectConfirmationCount(); $i++) {
    Output("&emsp;&emsp;Subject confirmation #" . ($i + 1) . ": ");
    Output("&emsp;&emsp;&emsp;Address: " . $reader->getSubjectConfirmationAddress($i));
    Output("&emsp;&emsp;&emsp;Data: " . $reader->getSubjectConfirmationData($i));
    Output("&emsp;&emsp;&emsp;Data type: " . $reader->getSubjectConfirmationDataType($i));
    Output("&emsp;&emsp;&emsp;ID: " . $reader->getSubjectConfirmationID($i));
    Output("&emsp;&emsp;&emsp;Method: " . $reader->getSubjectConfirmationMethod($i));
    Output("&emsp;&emsp;&emsp;Not before: " . $reader->getSubjectConfirmationNotBefore($i));
    Output("&emsp;&emsp;&emsp;Not after: " . $reader->getSubjectConfirmationNotOnOrAfter($i));
  }
}

function OutputAttributes($reader) {
  Output("&emsp;Attributes:");
  for ($i = 0; $i < $reader->getAttributeCount(); $i++) {
    Output("&emsp;&emsp;Attribute #" . ($i + 1) . ": ");
    Output("&emsp;&emsp;&emsp;Name: " . $reader->getAttributeName($i));
    Output("&emsp;&emsp;&emsp;Name Format: " . $reader->getAttributeNameFormat($i));
    Output("&emsp;&emsp;&emsp;Friendly Name: " . $reader->getAttributeFriendlyName($i));
    Output("&emsp;&emsp;&emsp;Statement Index: " . $reader->getAttributeStatementIndex($i));
    Output("&emsp;&emsp;&emsp;Value(s): " . $reader->getAttributeValues($i));
  }
}

function OutputConditions($reader) {
  Output("&emsp;Conditions:");
  for ($i = 0; $i < $reader->getConditionCount(); $i++) {
    Output("&emsp;&emsp;Condition #" . ($i + 1) . ": ");

    switch ($reader->getConditionConditionType($i)) {
      case SAMLREADER_CONDITIONCONDITIONTYPE_AUDIENCE_RESTRICTION:
        Output("&emsp;&emsp;&emsp;Condition type: Audience restriction");
        break;
      case SAMLREADER_CONDITIONCONDITIONTYPE_ONE_TIME_USE:
        Output("&emsp;&emsp;&emsp;Condition type: One-time use");
        break;
      case SAMLREADER_CONDITIONCONDITIONTYPE_PROXY_RESTRICTION:
        Output("&emsp;&emsp;&emsp;Condition type: Proxy restriction");
        break;
      case SAMLREADER_CONDITIONCONDITIONTYPE_NOT_BEFORE:
        Output("&emsp;&emsp;&emsp;Condition type: Not before");
        break;
      case SAMLREADER_CONDITIONCONDITIONTYPE_NOT_ON_OR_AFTER:
        Output("&emsp;&emsp;&emsp;Condition type: Not after");
        break;
      default:
        break;
    }

    Output("&emsp;&emsp;&emsp;Condition: " . $reader->getConditionCondition($i));
  }
}

function OutputStatements($reader) {
  Output("&emsp;Statements:");
  for ($i = 0; $i < $reader->getStatementCount(); $i++) {
    Output("&emsp;&emsp;Attribute #" . ($i + 1) . ": ");
    switch ($reader->getStatementStatementType($i)) { 
      case SAMLREADER_STATEMENTSTATEMENTTYPE_AUTHN:
        Output("&emsp;&emsp;&emsp;Type: AuthnStatement");
        Output("&emsp;&emsp;&emsp;Context class ref: " . $reader->getStatementAuthnContextClassRef($i));
        Output("&emsp;&emsp;&emsp;Instant: " . $reader->getStatementAuthnInstant($i));
        Output("&emsp;&emsp;&emsp;Session index: " . $reader->getStatementAuthnSessionIndex($i));
        Output("&emsp;&emsp;&emsp;Not on or after: " . $reader->getStatementAuthnSessionNotOnOrAfter($i));
        break;
      case SAMLREADER_STATEMENTSTATEMENTTYPE_ATTRIBUTE:
        Output("&emsp;&emsp;&emsp;Type: AttributeStatement (see attributes below)");
       break;
      SAMLREADER_STATEMENTSTATEMENTTYPE_AUTHZ_DECISION :
        Output("&emsp;&emsp;&emsp;Type: AuthzDecisionStatement");
        Output("&emsp;&emsp;&emsp;Actions: " . $reader->getStatementAuthzActions($i));
        Output("&emsp;&emsp;&emsp;Evidence: " . $reader->getStatementAuthzDecisionEvidence($i));
        switch ($reader->getStatementAuthzDecision($i)) {
          case SAMLREADER_STATEMENTAUTHZDECISION_PERMIT:
            Output("&emsp;&emsp;&emsp;Decision: Permit");
            break;
          case SAMLREADER_STATEMENTAUTHZDECISION_DENY:
            Output("&emsp;&emsp;&emsp;Decision: Deny");
            break;
          defaulr:
            Output("&emsp;&emsp;&emsp;Decision: Indeterminate");
            break;
        }
        break;
      default:
        Output("&emsp;&emsp;&emsp;Type: unknown");
        break;
    }
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $reader = new MySAMLReader();
    $certmgr = new SecureBlackBox_CertificateManager();

    try {
      // General options
      $input = $_REQUEST['input'];
      $vCertFile = $_REQUEST['vCertFile'];
      $dCertFile = $_REQUEST['dCertFile'];

      if (!empty($vCertFile)) {
        try {
          $certmgr->doImportFromFile($vCertFile, "");
        } catch (exception $e) {
          echo "<h2>Failed to load certificate</h2><p>" . $e->getMessage() . "</p>";
          return;
        }

        $reader->setCertCount(1);
        $reader->setCertHandle(0, $certmgr->getCertHandle());
      }

      if (!empty($dCertFile)) {
        try {
          $certmgr->doImportFromFile($dCertFile, $_REQUEST['dCertPass']);
        } catch (exception $e) {
          echo "<h2>Failed to load certificate</h2><p>" . $e->getMessage() . "</p>";
          return;
        }

        $reader->setDecryptionCertificateHandle($certmgr->getCertHandle());
      }

      Output("Starting message processing.");
      $reader->doOpenFile($input);

      // outputting the details
      Output("Message loaded successfully.");
      Output("General details: ");
      Output("&emsp;Destination: " . $reader->getMessageDestination());
      Output("&emsp;Issue instant: " . $reader->getMessageIssueInstant());
      Output("&emsp;ID: " . $reader->getMessageID());
      Output("&emsp;Issuer: " . $reader->getMessageIssuer());
      Output("&emsp;In response to: " . $reader->getMessageInResponseTo());

      switch($reader->getMessageContentType()) {
        case SAMLREADER_MESSAGECONTENTTYPE_NONE:
          Output("Message type: Unknown");
          break;
        case SAMLREADER_MESSAGECONTENTTYPE_ASSERTION_IDREQUEST:
          Output("Message type: AssertionIDRequest");
          break;
        case SAMLREADER_MESSAGECONTENTTYPE_SUBJECT_QUERY:
          Output("Message type: SubjectQuery");
          OutputSubjectQuery($reader);
          break;
        case SAMLREADER_MESSAGECONTENTTYPE_AUTHN_QUERY:
          Output("Message type: AuthnQuery");
          break;
        case SAMLREADER_MESSAGECONTENTTYPE_ATTRIBUTE_QUERY:
          Output("Message type: AttributeQuery");
          OutputAttributeQuery($reader);
          break;
        case SAMLREADER_MESSAGECONTENTTYPE_AUTHZ_DECISION_QUERY:
          Output("Message type: AuthzDecisionQuery");
          break;
        case SAMLREADER_MESSAGECONTENTTYPE_AUTHN_REQUEST:
          Output("Message type: AuthnRequest");
          OutputAuthnRequest($reader);
          break;
        case SAMLREADER_MESSAGECONTENTTYPE_MANAGE_NAME_IDREQUEST:
          Output("Message type: ManageNameIDRequest");
          break;
        case SAMLREADER_MESSAGECONTENTTYPE_LOGOUT_REQUEST:
          Output("Message type: LogoutRequest");
          OutputLogoutRequest($reader);
          break;
        case SAMLREADER_MESSAGECONTENTTYPE_NAME_IDMAPPING_REQUEST:
          Output("Message type: NameIDMappingRequest");
          break;
        case SAMLREADER_MESSAGECONTENTTYPE_ARTIFACT_RESOLVE:
          Output("Message type: ArtifactResolve");
          break;
        case SAMLREADER_MESSAGECONTENTTYPE_RESPONSE:
          Output("Message type: Response");
          OutputResponse($reader);
          break;
        default:
          break;
      }

      // global security props
      if ($reader->getMessageSigned() || $reader->getPinnedAssertionSigned()) {
        Output("The processed message was signed by its author.");
        if ($reader->getSigningCertHandle() != 0) {
          Output("Signing certificate: " . $reader->getSigningCertSubjectRDN());
        }
        else {
          Output("Signing certificate was not found");
        }
        Output("Signature validation result: " . $reader->getMessageSignatureValidationResult());
        Output("Digest method: " . $reader->getSecurityDigestMethod());
      }

      if ((($reader->getMessageContentType() == SAMLREADER_MESSAGECONTENTTYPE_ASSERTION) || ($reader->getMessageContentType() == SAMLREADER_MESSAGECONTENTTYPE_RESPONSE)) && ($reader->getPinnedAssertionAssertionType() == SAMLREADER_PINNEDASSERTIONASSERTIONTYPE_ENCRYPTED_ASSERTION)) {
        Output("The assertion was encrypted by its author.");
        Output("Encryption method: " . $reader->getSecurityEncryptionMethod());
      }
    }
    catch (exception $e) {
      echo "<h2>Creating Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>