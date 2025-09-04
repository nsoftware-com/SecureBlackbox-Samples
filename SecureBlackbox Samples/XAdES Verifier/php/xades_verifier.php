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
require_once('../include/secureblackbox_xadesverifier.php');
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
    <h2>XAdES Verififcation Demo</h2>

    <h3>General Options</h3>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
    </table><br />

    <input type=checkbox id="detached" name="detached" /><label for=detached>Detached</label><br />
    
    <table>
      <tr><td>Data File:</td><td><input type=text name=dataFile value=""></td></tr>
    </table><br />

    <input type="submit" value="Verify" />
  </form>
</div><br/>

<?php

class MyXAdESVerifier extends SecureBlackbox_XAdESVerifier
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
  
  function getXAdESVersionAsString($idx) {
    switch($this->getSignatureXAdESVersion($idx)) {
      case 1:  return "XAdES v1.1.1";              break;
      case 2:  return "XAdES v1.2.2";              break;
      case 3:  return "XAdES v1.3.2";              break;
      case 4:  return "XAdES v1.4.1 (aka v1.4.2)"; break;
      default: return "Unknown";                   break;
    }
  }
}

function translateLevel($lvl){
  switch($lvl){
    case aslGeneric:     return "Generic (XML-DSIG)";         break;
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

function translateSigValidationResult($value){
  switch($value){
    case XADESVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_VALID:               return "Valid";     break;
    case XADESVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_CORRUPTED:           return "Corrupted";     break;
    case XADESVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_SIGNER_NOT_FOUND:    return "Signer not found"; break;
    case XADESVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_FAILURE:             return "Failure";     break;
    case XADESVERIFIER_SIGNATURESIGNATUREVALIDATIONRESULT_REFERENCE_CORRUPTED: return "References corrupted";     break;
    default: return "Unknown";   break;
  }
}

function translateChainValidationResult($value){
  switch($value){
    case XADESVERIFIER_SIGNATURECHAINVALIDATIONRESULT_VALID:                return "Valid";     break;
    case XADESVERIFIER_SIGNATURECHAINVALIDATIONRESULT_VALID_BUT_UNTRUSTED:  return "Valid but untrusted";     break;
    case XADESVERIFIER_SIGNATURECHAINVALIDATIONRESULT_INVALID:              return "Invalid"; break;
    case XADESVERIFIER_SIGNATURECHAINVALIDATIONRESULT_CANT_BE_ESTABLISHED:  return "Can't be established";     break;
    default: return "Unknown";   break;
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $xadesverifier = new MyXAdESVerifier();
    $certmgr = new SecureBlackBox_CertificateManager();

    try {
      // General options
      $xadesverifier->setInputFile($_REQUEST['inputFile']);

      if(!empty($_REQUEST['detached'])){
        echo "detached";
        if(strcmp($_REQUEST['dataFile'], "") == 0){
          throw new Exception("A data file must be specified if the signature is detached.");
        }
        $xadesverifier->setDataFile($_REQUEST['dataFile']);
        $xadesverifier->setDataType(XADESVERIFIER_DATATYPE_BINARY);
        $xadesverifier->setDataURI(basename($_REQUEST['dataFile']));

        // Verification
        $xadesverifier->doVerifyDetached();
      } else {
        // Verification
        $xadesverifier->doVerify();
      }
      
      echo "<p>There were " . $xadesverifier->getSignatureCount() . " signatures.</p><br />";
      for ($x = 0; $x < $xadesverifier->getSignatureCount(); $x++) 
      {
        echo "<h3>Signature #" . ($x + 1) . "</h3><br /><table>";
        
        echo "<tr><td>Signature Validation Result:</td><td>" 
                                                . translateSigValidationResult($xadesverifier->getSignatureSignatureValidationResult($x))
                                                . "</td></tr>";
        echo "<tr><td>Chain Validation Result:</td><td>" 
                                                . translateChainValidationResult($xadesverifier->getSignatureChainValidationResult($x))
                                                . "</td></tr>";
        echo "<tr><td>XAdESVersion:</td><td>" . $xadesverifier->getXAdESVersionAsString($x) . "</td></tr>";
        echo "<tr><td>Level/XAdES form:</td><td>" . translateLevel($xadesverifier->getSignatureLevel($x)) . "</td></tr>";
        echo "</table><br />";
      }
      
      echo "<h3>References</h3><br />";
      echo "<table>";
      echo "<tr><td>Id</td><td>URI</td><td>RefType</td><td>Digest valid</td></tr>";
      echo $xadesverifier->getReferenceOutput();
      echo "</table><br />";
    }
    catch (exception $e) {
      echo "<h2>Verification Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>

