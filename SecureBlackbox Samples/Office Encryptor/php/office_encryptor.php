<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - Office Encryptor</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - Office Encryptor">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>Office Encryptor</h2>
<p>A lightweight encryptor of Office documents.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/secureblackbox_officeencryptor.php');
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
    <h2>Office Encryptor Demo</h2>
    
    <h3>General Options</h3>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Output File:</td><td><input type=text name=outputFile value=""></td></tr>
    </table>
    <br/>
    <br/>

    <b>Encryption Options</b><br/>
    <table>
      <tr>
        <td>Encryption Type:</td>
        <td>
          <select name="encType">
            <option value="OFFICEENCRYPTOR_ENCRYPTIONTYPE_DEFAULT">Default</option>
            <option value="OFFICEENCRYPTOR_ENCRYPTIONTYPE_BINARY_RC4">BinaryRC4</option>
            <option value="OFFICEENCRYPTOR_ENCRYPTIONTYPE_BINARY_RC4CRYPTO_API">BinaryRC4CryptoAPI</option>
            <option value="OFFICEENCRYPTOR_ENCRYPTIONTYPE_OPEN_XMLSTANDARD">OpenXMLStandard</option>
            <option value="OFFICEENCRYPTOR_ENCRYPTIONTYPE_OPEN_XMLAGILE">OpenXMLAgile</option>
            <option value="OFFICEENCRYPTOR_ENCRYPTIONTYPE_OPEN_DOCUMENT">OpenOffice</option>
          </select>
        </td>
      </tr>
      <tr>
        <td>Encryption Algorithm:</td>
        <td>
          <select name="encAlg">
            <option value=""></option>
            <option value="RC2">RC2</option>
            <option value="RC4">RC4</option>
            <option value="DES">DES</option>
            <option value="3DES">3DES</option>
            <option value="AES128">AES128</option>
            <option value="AES192">AES192</option>
            <option value="AES256">AES256</option>
            <option value="Blowfish">Blowfish</option>
          </select>
        </td>
      </tr>
      <tr><td>Password:</td><td><input type=password name=encpass value=""></td></tr>
    </table>
    <br/>
    <br/>

    <input type="submit" value="Encrypt" />
  </form>
</div><br/>

<?php

function translateEncryptionType($encType){
  switch($encType){
    case "OFFICEENCRYPTOR_ENCRYPTIONTYPE_BINARY_RC4":  return OFFICEENCRYPTOR_ENCRYPTIONTYPE_BINARY_RC4; break;
    case "OFFICEENCRYPTOR_ENCRYPTIONTYPE_BINARY_RC4CRYPTO_API":  return OFFICEENCRYPTOR_ENCRYPTIONTYPE_BINARY_RC4CRYPTO_API; break;
    case "OFFICEENCRYPTOR_ENCRYPTIONTYPE_OPEN_XMLSTANDARD":  return OFFICEENCRYPTOR_ENCRYPTIONTYPE_OPEN_XMLSTANDARD; break;
    case "OFFICEENCRYPTOR_ENCRYPTIONTYPE_OPEN_XMLAGILE":  return OFFICEENCRYPTOR_ENCRYPTIONTYPE_OPEN_XMLAGILE; break;
    case "OFFICEENCRYPTOR_ENCRYPTIONTYPE_OPEN_DOCUMENT":  return OFFICEENCRYPTOR_ENCRYPTIONTYPE_OPEN_DOCUMENT; break;
    default: return OFFICEENCRYPTOR_ENCRYPTIONTYPE_DEFAULT; break;
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $officeencryptor = new SecureBlackbox_OfficeEncryptor();
    
    try {
      // General options
      $officeencryptor->setInputFile($_REQUEST['inputFile']);
      $officeencryptor->setOutputFile($_REQUEST['outputFile']);
      
      // Encryption options
      $officeencryptor->setEncryptionType(translateEncryptionType($_REQUEST['encType']));
      $encAlg = $_REQUEST['encAlg'];
      if (!empty($encAlg)) {$officeencryptor->setEncryptionAlgorithm($encAlg);}
      $officeencryptor->setPassword($_REQUEST['encpass']);

      $officeencryptor->doEncrypt();
      echo "<h2>Office document successfully encrypted</h2>";
    }
    catch (exception $e) {
      echo "<h2>Encryption Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
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
