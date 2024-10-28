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
require_once('../include/secureblackbox_officeverifier.php');
require_once('../include/secureblackbox_certificatemanager.php');
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
    <h2>Office Verifier Demo</h2>
    
    <b>General Options</b><br/><br/>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
    </table>

    <br/><br/><b>Validation Options</b><br/><br/>

    <input type=checkbox name="ignoreChainValidationErrors" /><label for=ignoreChainValidationErrors>Ignore chain validation errors</label>
    <input type=checkbox name="forceCompleteChainValidation" /><label for=forceCompleteChainValidation>Force complete chain validation</label>
    <input type=checkbox name="performRevocationCheck" /><label for=performRevocationCheck>Perform revocation check</label>

    <br/><br/><br/>

    <b>Additional Certificates</b>
    <p>Enter the path(s) for any certificate(s), one per line. If a password is required add a semicolon followed by the password (e.g. C:\path\to\my.pfx;password).</p>
    <table>
      <tr><td>Known Certificates:</td><td><textarea style="font-family: Arial, sans-serif; width: 100%" name=knownCerts rows=10></textarea></td></tr>
      <tr><td>Trusted Certificates:</td><td><textarea style="font-family: Arial, sans-serif; width: 100%" name=trustedCerts rows=10></textarea></td></tr>
    </table>

    <input type="submit" value="Verify" />
  </form>
</div><br/>

<?php

function translateSignatureType($sigType){
  switch($sigType){
    case OFFICEVERIFIER_SIGNATURESIGNATURETYPE_BINARY_CRYPTO_API:  return "BinaryCryptoAPI";     break;
    case OFFICEVERIFIER_SIGNATURESIGNATURETYPE_BINARY_XML:         return "BinaryXML";     break;
    case OFFICEVERIFIER_SIGNATURESIGNATURETYPE_OPEN_XML:           return "OpenXML"; break;
    case OFFICEVERIFIER_SIGNATURESIGNATURETYPE_OPEN_XPS:           return "OpenXPS";     break;
    case OFFICEVERIFIER_SIGNATURESIGNATURETYPE_OPEN_DOCUMENT:      return "OpenOffice"; break;
    default: return "Unknown";   break;
  }
}

function translateDocSig($value) {
  if ($value) 
  {
    return "Document content is signed";
  }
  else
  {
    return "Document content is partially signed";
  }
}

function translateCore($value) {
  if ($value)
  {
    return "Document properties are signed";
  }
  else
  {
    return "Document properties are not signed";
  }
}

function translateOrigSig($value) {
  if ($value)
  {
    return "Signature origin is signed";
  }
  else
  {
    return "Signature origin is not signed";
  }
}

function translateSigValidationResult($value){
  switch($value){
    case OFFICEVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_VALID:             return "Valid";     break;
    case OFFICEVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_CORRUPTED:         return "Corrupted";     break;
    case OFFICEVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_SIGNER_NOT_FOUND:  return "Signer not found"; break;
    case OFFICEVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_FAILURE:           return "Failure";     break;
    default: return "Unknown";   break;
  }
}
function translateChainValidationResult($value){
  switch($value){
    case OFFICEVERIFIER_SIGNATURECHAINVALIDATIONRESULT_VALID:                return "Valid";     break;
    case OFFICEVERIFIER_SIGNATURECHAINVALIDATIONRESULT_VALID_BUT_UNTRUSTED:  return "Valid but untrusted";     break;
    case OFFICEVERIFIER_SIGNATURECHAINVALIDATIONRESULT_INVALID:              return "Invalid"; break;
    case OFFICEVERIFIER_SIGNATURECHAINVALIDATIONRESULT_CANT_BE_ESTABLISHED:  return "Can't be established";     break;
    default: return "Unknown";   break;
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $officeverifier = new SecureBlackbox_OfficeVerifier();
    $certmgr = new SecureBlackBox_CertificateManager();

    try {
      // General options
      $officeverifier->setInputFile($_REQUEST['inputFile']);

      // Additional options
      $officeverifier->setIgnoreChainValidationErrors(!empty($_REQUEST['ignoreChainValidationErrors']));
      $officeverifier->doConfig("ForceCompleteChainValidation=" . (!empty($_REQUEST['forceCompleteChainValidation']) ? "true" : "false"));
      $officeverifier->setRevocationCheck(!empty($_REQUEST['performRevocationCheck']) ? OFFICEVERIFIER_REVOCATIONCHECK_AUTO : OFFICEVERIFIER_REVOCATIONCHECK_NONE);

      // Known certificates
      $certPaths = trim($_REQUEST['knownCerts']);
      if (strlen($certPaths) > 0) {
        $knownCerts = explode("\r\n", $certPaths);
        $officeverifier->setKnownCertCount(count($knownCerts));
        for($x = 0; $x < count($knownCerts); $x++){
          $cert = ""; $pass = "";
          $delimitIdx = strpos($knownCerts[$x], ";");
          if($delimitIdx > 0){
            $cert = substr($knownCerts[$x], 0, $delimitIdx);
            $pass = substr($knownCerts[$x], $delimitIdx+1);
          } else {
            $cert = $knownCerts[$x];
          }

          try {
            $certmgr->doImportFromFile($cert, $pass);
          } catch (exception $e) {
            echo "<h2>Failed to load certificate</h2><p>" . $e->getMessage() . "</p>";
            return;
          }
          $officeverifier->setKnownCertHandle($x, $certmgr->getCertHandle());
        }
      }

      // Trusted certificates
      $certPaths = trim($_REQUEST['trustedCerts']);
      if (strlen($certPaths) > 0) {
        $trustedCerts = explode("\r\n", $certPaths);
        $officeverifier->setTrustedCertCount(count($trustedCerts));
        for($x = 0; $x < count($trustedCerts); $x++){
          $cert = ""; $pass = "";
          $delimitIdx = strpos($trustedCerts[$x], ";");
          if($delimitIdx > 0){
            $cert = substr($trustedCerts[$x], 0, $delimitIdx);
            $pass = substr($trustedCerts[$x], $delimitIdx+1);
          } else {
            $cert = $trustedCerts[$x];
          }

          try {
            $certmgr->doImportFromFile($cert, $pass);
          } catch (exception $e) {
            echo "<h2>Failed to load certificate</h2><p>" . $e->getMessage() . "</p>";
            return;
          }
          $officeverifier->setTrustedCertHandle($x, $certmgr->getCertHandle());
        }
      }

      // Verification
      $officeverifier->doVerify();

      echo "<p>There were " . $officeverifier->getSignatureCount() . " signatures.</p><br />";
      for ($x = 0; $x < $officeverifier->getSignatureCount(); $x++) 
      {
        echo "<h3>Signature #" . ($x + 1) . "</h3><br /><table>";
        
        echo "<tr><td>Signature type:</td><td>" . translateSignatureType($officeverifier->getSignatureSignatureType($x)) . "</td></tr>";
        echo "<tr><td>" . translateDocSig($officeverifier->getSignatureDocumentSigned($x)) . "</td><td></td></tr>";
        echo "<tr><td>" . translateCore($officeverifier->getSignatureCorePropertiesSigned($x)) . "</td><td></td></tr>";
        echo "<tr><td>" . translateOrigSig($officeverifier->getSignatureSignatureOriginSigned($x)) . "</td><td></td></tr>";
        echo "<tr><td>Signature Validation Result:</td><td>" 
                                                . translateSigValidationResult($officeverifier->getSignatureSignatureValidationResult($x))
                                                . "</td></tr>";
        echo "<tr><td>Chain Validation Result:</td><td>" 
                                                . translateChainValidationResult($officeverifier->getSignatureChainValidationResult($x))
                                                . "</td></tr>";
        echo "</table><br />";
      }
    }
    catch (exception $e) {
      echo "<h2>Verification Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>