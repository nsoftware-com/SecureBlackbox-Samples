<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - JWC Encryptor</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - JWC Encryptor">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>JWC Encryptor</h2>
<p>Use this example to learn about encrypting and decrypting messages in JSON format with SymmetricCrypto control.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/secureblackbox_symmetriccrypto.php');
require_once('../include/secureblackbox_cryptokeymanager.php');
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
    <h2>Symmetric crypto Demo</h2>
    
    <h3>General Options</h3>
    <label><input type ="radio" checked="checked" name="comtype" value="Encrypt" /> Encrypt </label>
    <label><input type ="radio" name="comtype" value="Decrypt" /> Decrypt </label>
    <br/>
    <br/>

    <table>
      <tr><td>Input:</td><td><input type=text name=inputData value=""></td></tr>
    </table>
    <br/>
    <br/>

    <label> <input type="checkbox" name="compact" value="1" size="25"/> Compact </label>
    <br/>
    <br/>

    <table>
      <tr><td>Password:</td><td><input type=password name=sPass value=""></td></tr>
    </table>
    <br/>
    <br/>

    <input type="submit" value="Encrypt/Decrypt" />
  </form>
</div><br/>

<?php
  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $crypto = new SecureBlackbox_SymmetricCrypto();
    $keymgr = new SecureBlackBox_CryptoKeyManager();
    
    try {
      // General options
      $inputData = $_REQUEST['inputData'];
      $compact = (isset($_REQUEST['compact']) && (($_REQUEST['compact'] == 'yes') || ($_REQUEST['compact'] == '1')));

      // Key from password
      $pass = trim($_REQUEST['sPass']);
      $keymgr->doDeriveKey(256, $pass, '');
      
      $crypto->setKeyHandle($keymgr->getKeyHandle());

      $crypto->setKeyIV("                ");

      $comtype = $_POST["comtype"];

      if ($comtype == "Encrypt")
      {
        if ($compact)
        {
          $crypto->setOutputEncoding(PUBLICKEYCRYPTO_OUTPUTENCODING_COMPACT);
        }
        else
        {
          $crypto->setOutputEncoding(PUBLICKEYCRYPTO_OUTPUTENCODING_JSON);
        }

        $outputData = $crypto->doEncrypt($inputData);
        echo "<h2>Encrypted token: " . $outputData . "</h2>";
      }
      else
      {
        if ($compact)
        {
          $crypto->setInputEncoding(PUBLICKEYCRYPTO_INPUTENCODING_COMPACT);
        }
        else
        {
          $crypto->setInputEncoding(PUBLICKEYCRYPTO_INPUTENCODING_JSON);
        }

        $outputData = $crypto->doDecrypt($inputData);
        echo "<h2>Decrypted string: " . $outputData . "</h2>";
      }
    }
    catch (exception $e) {
      echo "<h2>Encryption/decryption Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
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
