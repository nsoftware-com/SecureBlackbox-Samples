<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - PDF Verifier</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - PDF Verifier">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>PDF Verifier</h2>
<p>This small demo illustrates the use of the PDFVerifier control for processing PDF signatures.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/secureblackbox_pdfverifier.php');
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
    <h2>PDF Verifying Demo</h2>
    
    <b>General Options</b><br/><br/>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
    </table>

    <br/><br/><b>Validation Options</b><br/><br/>

    <input type=checkbox name="ignoreChainValidationErrors" /><label for=ignoreChainValidationErrors>Ignore chain validation errors</label>
    <input type=checkbox name="forceCompleteChainValidation" /><label for=forceCompleteChainValidation>Force complete chain validation</label>
    <input type=checkbox name="performRevocationCheck" /><label for=performRevocationCheck>Perform revocation check</label>
    <input type=checkbox name="deepValidation" /><label for=deepValidation>Deep validation</label>

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

function translateSigValidationResult($res){
  switch($res){
    case PDFVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_VALID:             return "Valid";          break;
    case PDFVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_CORRUPTED:         return "Corrupted";      break;
    case PDFVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_SIGNER_NOT_FOUND:  return "SignerNotFound"; break;
    case PDFVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_FAILURE:           return "Failure";        break;
    default: return "Unknown";        break;
  }
}

function translateChainValidationResult($res){
  switch($res){
    case PDFVERIFIER_SIGNATURECHAINVALIDATIONRESULT_VALID:                return "Valid";             break;
    case PDFVERIFIER_SIGNATURECHAINVALIDATIONRESULT_VALID_BUT_UNTRUSTED:  return "ValidButUntrusted"; break;
    case PDFVERIFIER_SIGNATURECHAINVALIDATIONRESULT_INVALID:              return "Invalid";           break;
    case PDFVERIFIER_SIGNATURECHAINVALIDATIONRESULT_CANT_BE_ESTABLISHED:  return "CantBeEstablished";           break;
    default: return "Unknown"; break;
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $pdfverifier = new SecureBlackbox_PDFVerifier();
    $certmgr = new SecureBlackBox_CertificateManager();

    try {
      // General options
      $pdfverifier->setInputFile($_REQUEST['inputFile']);

      // Additional options
      $pdfverifier->setIgnoreChainValidationErrors(!empty($_REQUEST['ignoreChainValidationErrors']));
      $pdfverifier->doConfig("ForceCompleteChainValidation=" . (!empty($_REQUEST['forceCompleteChainValidation']) ? "True" : "False"));
      $pdfverifier->setRevocationCheck(!empty($_REQUEST['performRevocationCheck']) ? 1 : 0);

      // Known certificates
      $certPaths = trim($_REQUEST['knownCerts']);
      if (strlen($certPaths) > 0) {
        $knownCerts = explode("\r\n", $certPaths);
        $pdfverifier->setKnownCertCount(count($knownCerts));
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
          $pdfverifier->setKnownCertHandle($x, $certmgr->getCertHandle());
        }
      }

      // Trusted certificates
      $certPaths = trim($_REQUEST['trustedCerts']);
      if (strlen($certPaths) > 0) {
        $trustedCerts = explode("\r\n", $certPaths);
        $pdfverifier->setTrustedCertCount(count($trustedCerts));
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
          $pdfverifier->setTrustedCertHandle($x, $certmgr->getCertHandle());
        }
      }

      // Verification
      $pdfverifier->doVerify();

      echo "<h2>Verification Successful</h2>";
      echo "<p>There were " . $pdfverifier->getSignatureCount() . " signatures.</p><br />";
      for($x = 0; $x < $pdfverifier->getSignatureCount(); $x++){
        echo "<h3>Signature #" . ($x+1) . "</h3><br /><table>";
        echo "<tr><td>Subject RDN:</td><td>"     . $pdfverifier->getSignatureSubjectRDN($x)          . "</td></tr>";
        echo "<tr><td>Timestamp:</td><td>"       . $pdfverifier->getSignatureClaimedSigningTime($x)  . "</td></tr>";
        echo "<tr><td>Signature Validation Result:</td><td>" 
                                                . translateSigValidationResult($pdfverifier->getSignatureSignatureValidationResult($x))
                                                . "</td></tr>";
        echo "<tr><td>Chain Validation Result:</td><td>" 
                                                . translateChainValidationResult($pdfverifier->getSignatureChainValidationResult($x))
                                                . "</td></tr>";
        echo "</table><br />";
      }
    }
    catch (exception $e) {
      echo "<h2>Verification Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>
<br/>
<br/>
<br/>
<hr/>
NOTE: These pages are simple demos, and by no means complete applications.  They
are intended to illustrate the usage of the SecureBlackbox objects in a simple,
straightforward way.  What we are hoping to demonstrate is how simple it is to
program with our components.  If you want to know more about them, or if you have
questions, please visit <a href="http://www.nsoftware.com/?demopg-SBPHA" target="_blank">www.nsoftware.com</a> or
contact our technical <a href="http://www.nsoftware.com/support/">support</a>.
<br/>
<br/>
Copyright (c) 2023 /n software inc.
<br/>
<br/>
</div>

<div id="footer">
<center>
SecureBlackbox 2022 - Copyright (c) 2023 /n software inc. - For more information, please visit our website at <a href="http://www.nsoftware.com/?demopg-SBPHA" target="_blank">www.nsoftware.com</a>.
</center>
</div>

</body>
</html>

<?php if ($sendBuffer) ob_end_flush(); else ob_end_clean(); ?>
