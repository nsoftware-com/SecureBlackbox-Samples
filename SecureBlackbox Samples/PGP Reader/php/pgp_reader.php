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
require_once('../include/secureblackbox_pgpreader.php');
require_once('../include/secureblackbox_pgpkeyring.php');
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
    <h2>PGP Reader Demo</h2>
    
    <h3>General Options</h3>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Output File:</td><td><input type=text name=outputFile value=""></td></tr>
    </table>
    <br/>
    <br/>

    <b>Keys</b><br/>
    <table>
      <tr><td>Public keyring:</td><td><input type=text name=publicKey value=""></td></tr>
      <tr><td>Secret keyring:</td><td><input type=text name=secretKey value=""></td></tr>
      <tr><td>Key Passphrase:</td><td><input type=password name=sPass value=""></td></tr>
    </table>
    <br/>
    <br/>

    <input type="submit" value="Decrypt and Verify" />
  </form>
</div><br/>

<?php
class MyPGPReader extends SecureBlackbox_PGPReader
{
  private $password;
  
  function setPassword($value){
    $this->password = $value;

    return 0;
  }
  
  function fireKeyPassphraseNeeded($param){
    echo $this->password;
    $param['passphrase'] = $this->password;

    return $param;
  }
}

function translateSigValidationResult($res){
  switch($res){
    case PGPREADER_SIGNATUREVALIDITY_VALID:  return "Valid";  break;
    case PGPREADER_SIGNATUREVALIDITY_CORRUPTED:  return "Corrupted";  break;
    case PGPREADER_SIGNATUREVALIDITY_UNKNOWN_ALGORITHM:  return "Unknown signing algorithm";  break;
    case PGPREADER_SIGNATUREVALIDITY_NO_KEY:  return "Signing key not found, unable to verify";  break;
    default: return "Unknown";  break;
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $pgpreader = new MyPGPReader();
    $pgpkeyring = new SecureBlackbox_PGPKeyring();
    
    try {
      // General options
      $inputFile = $_REQUEST['inputFile'];
      $outputFile = $_REQUEST['outputFile'];
      
      // Keys
      $pgpkeyring->doImportFromFile($_REQUEST['publicKey']);
      $pgpkeyring->doImportFromFile($_REQUEST['secretKey']);

      $pgpreader->setVerifyingKeyCount($pgpkeyring->getKeyCount());
      for($x = 0; $x < $pgpkeyring->getKeyCount(); $x++)
      {
        $pgpreader->setVerifyingKeyHandle($x, $pgpkeyring->getKeyHandle($x));
      }

      $pgpreader->setDecryptingKeyCount(0);
      for($x = 0; $x < $pgpkeyring->getKeyCount(); $x++)
      {
        if ($pgpkeyring->getKeyIsSecret($x))
        {
          $pgpreader->setDecryptingKeyCount($pgpreader->getDecryptingKeyCount() + 1);
          $pgpreader->setDecryptingKeyHandle($pgpreader->getDecryptingKeyCount() - 1, $pgpkeyring->getKeyHandle($x));
        }
      }

      $pgpreader->setPassword($_REQUEST['sPass']);

      $pgpreader->setInputfile($inputFile);
      $pgpreader->setOutputfile($outputFile);
      $pgpreader->doDecryptAndVerify();
      
      echo "<p>There were " . $pgpreader->getSignatureCount() . " signatures.</p><br />";
      for($x = 0; $x < $pgpreader->getSignatureCount(); $x++){
        echo "<h3>Signature #" . ($x+1) . "</h3><br /><table>";
        
        $userID = "Unknown Key";
        for($y = 0; $y < $pgpkeyring->getPublicKeyCount(); $y++){
          if (!$pgpkeyring->getPublicKeyIsSubkey($y) && $pgpkeyring->getPublicKeyKeyID($y) == $pgpreader->getSignatureSignerKeyID($x)) {
            $userID = $pgpkeyring->getPublicKeyUsername($y);
          }
        }

        echo "<tr><td>Signer:</td><td>"          . $userID . "</td></tr>";
        echo "<tr><td>Signature Validation Result:</td><td>" 
                                                . translateSigValidationResult($pgpreader->getSignatureValidity($x))
                                                . "</td></tr>";
        echo "</table><br/><br/>";
      }

      echo "<h2>The file was decrypted successfully</h2>";
    }
    catch (exception $e) {
      echo "<h2>Signing Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>