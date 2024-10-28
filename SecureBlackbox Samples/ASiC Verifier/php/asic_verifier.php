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

require_once('../include/secureblackbox_const.php');

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
    case aslGeneric:     return "Generic";         break;
    case aslBaselineB:   return "Baseline B";       break;
    case aslBaselineT:   return "Baseline T";       break;
    case aslBaselineLT:  return "Baseline LT";      break;
    case aslBaselineLTA: return "Baseline LTA";     break;
    case aslBES:         return "BES";             break;
    case aslEPES:        return "EPES";            break;
    case aslT:           return "T";               break;
    case aslC:           return "C";               break;
    case aslX:           return "X";               break;
    case aslXType1:      return "XType1";          break;
    case aslXType2:      return "XType2";          break;
    case aslXL:          return "X-L";              break;
    case aslXLType1:     return "XLType1";         break;
    case aslXLType2:     return "XLType2";         break;
    case aslA:           return "A";               break;
    case aslExtendedBES: return "Extended BES";     break;
    case aslExtendedEPES: return "Extended EPES";    break;
    case aslExtendedT:   return "Extended T";       break;
    case aslExtendedC:   return "Extended C";       break;
    case aslExtendedX:   return "Extended X";       break;
    case aslExtendedXType1: return "Extended XType1";  break;
    case aslExtendedXType2: return "Extended XType2";  break;
    case aslExtendedXLong:  return "Extended X-Long";       break;
    case aslExtendedXL:  return "Extended X-L";       break;
    case aslExtendedXLType1: return "Extended XLType1"; break;
    case aslExtendedXLType2: return "Extended XLType2"; break;
    case aslExtendedA:   return "Extended A";       break;
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