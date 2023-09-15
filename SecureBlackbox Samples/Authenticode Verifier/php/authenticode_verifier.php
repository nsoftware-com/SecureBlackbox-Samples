<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - Authenticode Verifier</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - Authenticode Verifier">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>Authenticode Verifier</h2>
<p>A simple authenticode verifier based on the AuthenticodeVerifier component. Use it to verify signed EXE and DLL files.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
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
