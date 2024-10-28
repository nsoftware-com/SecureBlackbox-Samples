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
require_once('../include/secureblackbox_authenticodeverifier.php');
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
    <h2>Authenticode Signing Demo</h2>
    
    <h3>General Options</h3>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
    </table>
    <br/>
    <br/>

    <input type="submit" value="Verify" />
  </form>
</div><br/>

<?php

function translateSigValidationResult($res){
  switch($res){
    case AUTHENTICODEVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_VALID:            return "Valid";          break;
    case AUTHENTICODEVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_CORRUPTED:        return "Corrupted";      break;
    case AUTHENTICODEVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_SIGNER_NOT_FOUND: return "SignerNotFound"; break;
    case AUTHENTICODEVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_FAILURE:          return "Failure";        break;
    default:                                                                       return "Unknown";        break;
  }
}
function translateChainValidationResult($res){
  switch($res){
    case AUTHENTICODEVERIFIER_SIGNATURECHAINVALIDATIONRESULT_VALID:               return "Valid";             break;
    case AUTHENTICODEVERIFIER_SIGNATURECHAINVALIDATIONRESULT_VALID_BUT_UNTRUSTED: return "ValidButUntrusted"; break;
    case AUTHENTICODEVERIFIER_SIGNATURECHAINVALIDATIONRESULT_INVALID:             return "Invalid";           break;
    default:                                                                      return "CantBeEstablished"; break;
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $authenticodeverifier = new SecureBlackbox_AuthenticodeVerifier();
    
    try {
      // General options
      $authenticodeverifier->setInputFile($_REQUEST['inputFile']);
      
      $authenticodeverifier->doVerify();

      if (!$authenticodeverifier->getSigned())
      {
        echo "<h2>The file is not singed</h2>";
      }
      else
      {
        echo "<h2>Verification Successful</h2>";
        echo "<p>There were " . $authenticodeverifier->getSignatureCount() . " signatures.</p><br />";
        for ($x = 0; $x < $authenticodeverifier->getSignatureCount(); $x++) 
        {
          echo "<h3>Signature #" . $x . "</h3><br /><table>";
        
          echo "<tr><td>Hash algorithm:</td><td>" . $authenticodeverifier->getSignatureHashAlgorithm($x) . "</td></tr>";
          echo "<tr><td>Description:</td><td>"    . $authenticodeverifier->getSignatureDescription($x)   . "</td></tr>";
          echo "<tr><td>URL:</td><td>"            . $authenticodeverifier->getSignatureURL($x)           . "</td></tr>";
          echo "<tr><td>Signature Validation Result:</td><td>" 
                                                  . translateSigValidationResult($authenticodeverifier->getSignatureSignatureValidationResult($x))
                                                  . "</td></tr>";
          echo "<tr><td>Chain Validation Result:</td><td>" 
                                                  . translateChainValidationResult($authenticodeverifier->getSignatureChainValidationResult($x))
                                                  . "</td></tr>";
          echo "</table><br />";
        }
      }
    }
    catch (exception $e) {
      echo "<h2>Verification Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
      echo "<p>" . $authenticodesigner->getStatementType() . "</p>";
    }
  }
?>