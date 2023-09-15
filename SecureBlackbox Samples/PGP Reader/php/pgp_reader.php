<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - PGP Reader</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - PGP Reader">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>PGP Reader</h2>
<p>Use this easy-to-use example to learn about integrating PGP decryption and verification into your application.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
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
      $pgpkeyring->doLoad($_REQUEST['publicKey'], $_REQUEST['secretKey']);

      $pgpreader->setVerifyingKeyCount($pgpkeyring->getPublicKeyCount());
      for($x = 0; $x < $pgpkeyring->getPublicKeyCount(); $x++)
      {
        $pgpreader->setVerifyingKeyHandle($x, $pgpkeyring->getPublicKeyHandle($x));
      }

      $pgpreader->setDecryptingKeyCount($pgpkeyring->getSecretKeyCount());
      for($x = 0; $x < $pgpkeyring->getSecretKeyCount(); $x++)
      {
        $pgpreader->setDecryptingKeyHandle($x, $pgpkeyring->getSecretKeyHandle($x));
      }

      $pgpreader->setDecryptingKeyPassphrase(0, $_REQUEST['sPass']);

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
