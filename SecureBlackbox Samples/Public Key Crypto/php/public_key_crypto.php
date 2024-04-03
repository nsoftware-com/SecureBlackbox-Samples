<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - Public Key Crypto</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - Public Key Crypto">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>Public Key Crypto</h2>
<p>Use this example to learn about sign and verify with PublicKeyCrypto control.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/secureblackbox_publickeycrypto.php');
require_once('../include/secureblackbox_cryptokeymanager.php');
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
    <h2>Public key crypto Demo</h2>
    
    <h3>General Options</h3>
    <label><input type ="radio" checked="checked" name="comtype" value="Sign" /> Sign </label>
    <label><input type ="radio" name="comtype" value="Verify" /> Verify </label>
    <br/>
    <br/>

    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Output/Signature File:</td><td><input type=text name=outputFile value=""></td></tr>
    </table>
    <br/>
    <br/>

    <table>
      <tr>
        <td>Encoding:</td>
        <td>
          <select name="encoding">
            <option value="Binary">Binary</option>
            <option value="Base64">Base64</option>
            <option value="Compact">Compact</option>
            <option value="JSON">JSON</option>
          </select>
        </td>
      </tr>
    </table>
    <br/>
    <br/>

    <b>Signing Certificate</b>
    <table>
      <tr><td>Certificate File:</td><td><input type=text name=sCertFile value=""></td></tr>
      <tr><td>Password:</td><td><input type=password name=sCertPass value=""></td></tr>
    </table>
    <br/>
    <br/>

    <input type="submit" value="Sign/Verify" />
  </form>
</div><br/>

<?php
  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $crypto = new SecureBlackbox_PublicKeyCrypto();
    $certmgr = new SecureBlackBox_CertificateManager();
    $keymgr = new SecureBlackBox_CryptoKeyManager();
    
    try {
      // General options
      $inputFile = $_REQUEST['inputFile'];
      $outputFile = $_REQUEST['outputFile'];

      $comtype = $_POST["comtype"];
      if ($comtype == "Sign")
      {
        switch(trim($_REQUEST['encoding'])){
          case "Binary":  $crypto->setOutputEncoding(PUBLICKEYCRYPTO_OUTPUTENCODING_BINARY); break;
          case "Base64":  $crypto->setOutputEncoding(PUBLICKEYCRYPTO_OUTPUTENCODING_BASE_64); break;
          case "Compact":  $crypto->setOutputEncoding(PUBLICKEYCRYPTO_OUTPUTENCODING_COMPACT); break;
          case "JSON":  $crypto->setOutputEncoding(PUBLICKEYCRYPTO_OUTPUTENCODING_JSON); break;
          default: $crypto->setOutputEncoding(PUBLICKEYCRYPTO_OUTPUTENCODING_DEFAULT); break;
        }
      }
      else
      {
        switch(trim($_REQUEST['encoding'])){
          case "Binary":  $crypto->setInputEncoding(PUBLICKEYCRYPTO_INPUTENCODING_BINARY); break;
          case "Base64":  $crypto->setInputEncoding(PUBLICKEYCRYPTO_INPUTENCODING_BASE_64); break;
          case "Compact":  $crypto->setInputEncoding(PUBLICKEYCRYPTO_INPUTENCODING_COMPACT); break;
          case "JSON":  $crypto->setInputEncoding(PUBLICKEYCRYPTO_INPUTENCODING_JSON); break;
          default: $crypto->setInputEncoding(PUBLICKEYCRYPTO_INPUTENCODING_DEFAULT); break;
        }
      }

      // Signing options
      $certmgr->doImportFromFile($_REQUEST['sCertFile'], $_REQUEST['sCertPass']);
      $keymgr->setCertHandle($certmgr->getCertHandle());
      $keymgr->doImportFromCert();
      $crypto->setKeyHandle($keymgr->getKeyHandle());

      if ($comtype == "Sign")
      {
        $crypto->doSignFile($inputFile, $outputFile, TRUE);
        echo "<h2>The file was signed successfully</h2>";
      }
      else
      {
        $crypto->doVerifyDetachedFile($inputFile, $outputFile);

        switch($crypto->getSignatureValidationResult())
        {
          case PUBLICKEYCRYPTO_SIGNATUREVALIDATIONRESULT_VALID:  echo "<h2>Signature validated successfully</h2>";          break;
          case PUBLICKEYCRYPTO_SIGNATUREVALIDATIONRESULT_CORRUPTED:  echo "<h2>Signature is invalid</h2>";      break;
          case PUBLICKEYCRYPTO_SIGNATUREVALIDATIONRESULT_SIGNER_NOT_FOUND:  echo "<h2>Signer not found</h2>"; break;
          case PUBLICKEYCRYPTO_SIGNATUREVALIDATIONRESULT_FAILURE:  echo "<h2>Signature verification failed</h2>";        break;
          default: echo "<h2>Unknown</h2>";        break;
        }
      }
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
