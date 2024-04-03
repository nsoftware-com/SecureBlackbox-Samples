<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - ASiC Verifier</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - ASiC Verifier">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>ASiC Verifier</h2>
<p>A simple ASiC verifier created with the ASiCVerifier component. Use it to verify ASiC signatures.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/secureblackbox_asicverifier.php');
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
    <h2>ASiC Verifying Demo</h2>
    
    <h3>General Options</h3>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Extract Path:</td><td><input type=text name=extractPath value=""></td></tr>
    </table>

    <h3>Validation Options</h3><br />
    <b>Extraction Mode:</b><br />
    <div style="display: inline-block; margin: .25em 0;">
      <select name=extractMode id="extractMode">
        <option value="0" selected="selected">None</option>
        <option value="1">All</option>
        <option value="2">Signed</option>
        <option value="3">Signed and Valid</option>
      </select>
    </div><br /><br />
    
    <input type=checkbox name="ignoreChainValidationErrors" /><label for=ignoreChainValidationErrors>Ignore chain validation errors</label>
    <input type=checkbox name="forceCompleteChainValidation" /><label for=forceCompleteChainValidation>Force complete chain validation</label>
    <input type=checkbox name="performRevocationCheck" /><label for=performRevocationCheck>Perform revocation check</label>

    <br /><br /><br />

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
    case 1:  return "CAdES";     break;
    case 2:  return "XAdES";     break;
    case 3:  return "Timestamp"; break;
    default: return "Unknown";   break;
  }
}
function translateLevel($lvl){
  switch($lvl){
    case 1:  return "BES";             break;
    case 2:  return "EPES";            break;
    case 3:  return "T";               break;
    case 4:  return "C";               break;
    case 5:  return "XType1";          break;
    case 6:  return "XType2";          break;
    case 7:  return "XLType1";         break;
    case 8:  return "XLType2";         break;
    case 9:  return "BaselineB";       break;
    case 10: return "BaselineT";       break;
    case 11: return "BaselineLT";      break;
    case 12: return "BaselineLTA";     break;
    case 13: return "ExtendedBES";     break;
    case 14: return "ExtendedEPES";    break;
    case 15: return "ExtendedT";       break;
    case 16: return "ExtendedC";       break;
    case 17: return "ExtendedXType1";  break;
    case 18: return "ExtendedXType2";  break;
    case 19: return "ExtendedXLType1"; break;
    case 20: return "ExtendedXLType2"; break;
    case 21: return "A";               break;
    case 22: return "ExtendedA";       break;
    default: return "Unknown";         break;
  }
}
function translateSigValidationResult($res){
  switch($res){
    case 0:  return "Valid";          break;
    case 2:  return "Corrupted";      break;
    case 3:  return "SignerNotFound"; break;
    case 4:  return "Failure";        break;
    default: return "Unknown";        break;
  }
}
function translateChainValidationResult($res){
  switch($res){
    case 0:  return "Valid";             break;
    case 1:  return "ValidButUntrusted"; break;
    case 2:  return "Invalid";           break;
    default: return "CantBeEstablished"; break;
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $asicverifier = new SecureBlackbox_ASiCVerifier();
    $certmgr = new SecureBlackBox_CertificateManager();

    try {
      // General options
      $asicverifier->setInputFile($_REQUEST['inputFile']);
      $asicverifier->setOutputPath($_REQUEST['extractPath']);

      // Additional options
      $asicverifier->setExtractionMode($_REQUEST['extractMode']);
      $asicverifier->setIgnoreChainValidationErrors(!empty($_REQUEST['ignoreChainValidationErrors']));
      $asicverifier->doConfig("ForceCompleteChainValidation=" . (!empty($_REQUEST['forceCompleteChainValidation']) ? "True" : "False"));
      $asicverifier->setRevocationCheck(!empty($_REQUEST['performRevocationCheck']) ? 1 : 0);

      // Known certificates
      $certPaths = trim($_REQUEST['knownCerts']);
      if (strlen($certPaths) > 0) {
        $knownCerts = explode("\r\n", $certPaths);
        $asicverifier->setKnownCertCount(count($knownCerts));
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
          $asicverifier->setKnownCertHandle($x, $certmgr->getCertHandle());
        }
      }

      // Trusted certificates
      $certPaths = trim($_REQUEST['trustedCerts']);
      if (strlen($certPaths) > 0) {
        $trustedCerts = explode("\r\n", $certPaths);
        $asicverifier->setTrustedCertCount(count($trustedCerts));
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
          $asicverifier->setTrustedCertHandle($x, $certmgr->getCertHandle());
        }
      }

      // Verification
      $asicverifier->doVerify();
      
      echo "<h2>Verification Successful</h2>";
      echo "<p>There were " . $asicverifier->getSignatureCount() . " signatures.</p><br />";
      for($x = 0; $x < $asicverifier->getSignatureCount(); $x++){
        echo "<h3>Signature #" . $x . "</h3><br /><table>";
        
        echo "<tr><td>Level:</td><td>"          . translateLevel($asicverifier->getSignatureLevel($x))                            . "</td></tr>";
        echo "<tr><td>Signature Type:</td><td>" . translateSignatureType($asicverifier->getSignatureSignatureType($x)) . "</td></tr>";
        echo "<tr><td>Issuer RDN:</td><td>"     . $asicverifier->getSignatureIssuerRDN($x)                             . "</td></tr>";
        if ($asicverifier->getSignatureSignatureType($x) == 3) {
          echo "<tr><td>Timestamp:</td><td>"      . $asicverifier->getSignatureValidatedSigningTime($x)                                  . "</td></tr>";
        }
        echo "<tr><td>Signed Files:</td><td>"   . $asicverifier->getSignatureSignedFiles($x)                           . "</td></tr>";
        echo "<tr><td>Signature Validation Result:</td><td>" 
                                                . translateSigValidationResult($asicverifier->getSignatureSignatureValidationResult($x))
                                                . "</td></tr>";
        echo "<tr><td>Chain Validation Result:</td><td>" 
                                                . translateChainValidationResult($asicverifier->getSignatureChainValidationResult($x))
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
Copyright (c) 2024 /n software inc.
<br/>
<br/>
</div>

<div id="footer">
<center>
SecureBlackbox 2022 - Copyright (c) 2024 /n software inc. - For more information, please visit our website at <a href="http://www.nsoftware.com/?demopg-SBPHA" target="_blank">www.nsoftware.com</a>.
</center>
</div>

</body>
</html>

<?php if ($sendBuffer) ob_end_flush(); else ob_end_clean(); ?>
