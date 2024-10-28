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
require_once('../include/secureblackbox_xmlverifier.php');
require_once('../include/secureblackbox_certificatemanager.php');
require_once('../include/secureblackbox_xadesverifier.php');
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
    <h2>XML Verififcation Demo</h2>

    <h3>General Options</h3>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
    </table><br />

    <input type=checkbox id="detached" name="detached" /><label for=detached>Detached</label><br />
    
    <table>
      <tr><td>Output/Data File:</td><td><input type=text name=dataFile value=""></td></tr>
    </table><br />

    <p>A certificate is only required if the key was not included with the signature.</p>
    <table>
      <tr><td>Certificate:</td><td><input type=text name=certFile value=""></td></tr>
      <tr><td>Password:</td><td><input type=password name=certPass value=""></td></tr>
    </table><br />

    <input type="submit" value="Verify" />
  </form>
</div><br/>

<?php

class MyXMLVerifier extends SecureBlackbox_XMLVerifier
{
  private $referenceOutput;
  
  function getReferenceOutput(){
    return $this->referenceOutput;
  }
  
  function fireReferenceValidated($param){
    $this->referenceOutput .= "<tr>";
    $this->referenceOutput .= "<td>" . $param['id']                               . "</td>";
    $this->referenceOutput .= "<td>" . $param['uri']                              . "</td>";
    $this->referenceOutput .= "<td>" . $param['reftype']                          . "</td>";
    $this->referenceOutput .= "<td>" . ($param['digestvalid'] ? "true" : "false") . "</td>";
    $this->referenceOutput .= "</tr>";  

    return $param;
  }
}

function translateSigValidationResult($value){
  switch($value){
    case XMLVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_VALID:               return "Valid";     break;
    case XMLVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_CORRUPTED:           return "Corrupted";     break;
    case XMLVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_SIGNER_NOT_FOUND:    return "Signer not found"; break;
    case XMLVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_FAILURE:             return "Failure";     break;
    case XMLVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_REFERENCE_CORRUPTED: return "References corrupted";     break;
    default: return "Unknown";   break;
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $xmlverifier = new MyXMLVerifier();
    $certmgr = new SecureBlackBox_CertificateManager();

    try {
      // General options
      $xmlverifier->setInputFile($_REQUEST['inputFile']);
      
      if (!empty($_REQUEST['certFile'])) {
        try {
          $certmgr->doImportFromFile($_REQUEST['certFile'], $_REQUEST['certPass']);
        } catch (exception $e) {
          echo "<h2>Failed to load certificate</h2><p>" . $e->getMessage() . "</p>";
          return;
        }

        $xmlverifier->setKnownCertCount(1);
        $xmlverifier->setKnownCertHandle(0, $certmgr->getCertHandle());
      }

      if(!empty($_REQUEST['detached'])){
        if(strcmp($_REQUEST['dataFile'], "") == 0){
          throw new Exception("A data file must be specified if the signature is detached.");
        }
        $xmlverifier->setDataFile($_REQUEST['dataFile']);
        $xmlverifier->setDataType(XMLVERIFIER_DATATYPE_BINARY);
        $xmlverifier->setDataURI(basename($_REQUEST['dataFile']));

        // Verification
        $xmlverifier->doVerifyDetached();
      } else {
        // Verification
        $xmlverifier->doVerify();
      }
     
      echo "<p>There were " . $xmlverifier->getSignatureCount() . " signatures.</p><br />";
      for ($x = 0; $x < $xmlverifier->getSignatureCount(); $x++) 
      {
        echo "<h3>Signature #" . ($x + 1) . "</h3><br /><table>";
        
        echo "<tr><td>Signature Validation Result:</td><td>" 
                                                . translateSigValidationResult($xmlverifier->getSignatureSignatureValidationResult($x))
                                                . "</td></tr>";
        echo "</table><br />";
      }

      echo "<h3>References</h3><br />";
      echo "<table>";
      echo "<tr><td>Id</td><td>URI</td><td>RefType</td><td>Digest valid</td></tr>";
      echo $xmlverifier->getReferenceOutput();
      echo "</table><br />";
    }
    catch (exception $e) {
      echo "<h2>Verification Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>