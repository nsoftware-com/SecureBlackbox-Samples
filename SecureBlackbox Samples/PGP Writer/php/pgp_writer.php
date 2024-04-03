<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - PGP Writer</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - PGP Writer">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>PGP Writer</h2>
<p>A simple PGP encryptor-plus-verifier.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/secureblackbox_pgpwriter.php');
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
    <h2>PGP Writer Demo</h2>
    
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

    <input type="submit" value="Encrypt and Sign" />
  </form>
</div><br/>

<?php
class MyPGPWriter extends SecureBlackbox_PGPWriter
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

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $pgpwriter = new MyPGPWriter();
    $pgpkeyring = new SecureBlackbox_PGPKeyring();
    
    try {
      // General options
      $inputFile = $_REQUEST['inputFile'];
      $outputFile = $_REQUEST['outputFile'];
      
      // Keys
      $pgpkeyring->doLoad($_REQUEST['publicKey'], $_REQUEST['secretKey']);

      if ($pgpkeyring->getPublicKeyCount() > 0)
      {
        $pgpwriter->setEncryptingKeyCount(1);
        $pgpwriter->setEncryptingKeyHandle(0, $pgpkeyring->getPublicKeyHandle(0));
      }
      else
      {
        echo "<h2>Public keys not found</h2>";
        return 2;
      }

      if ($pgpkeyring->getSecretKeyCount() > 0)
      {
        $pgpwriter->setSigningKeyCount(1);
        $pgpwriter->setSigningKeyHandle(0, $pgpkeyring->getSecretKeyHandle(0));
      }
      else
      {
        echo "<h2>Secret keys not found</h2>";
        return 2;
      }

      $pgpwriter->setSigningKeyPassphrase(0, $_REQUEST['sPass']);

      $pgpwriter->setInputfile($inputFile);
      $pgpwriter->setOutputfile($outputFile);
      $pgpwriter->doEncryptAndSign();
      echo "<h2>The files were encrypted and signed successfully</h2>";
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
