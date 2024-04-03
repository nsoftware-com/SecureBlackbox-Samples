<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - XML Encryptor</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - XML Encryptor">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>XML Encryptor</h2>
<p>A tiny XML encryption example.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/secureblackbox_xmlencryptor.php');
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
    <h2>XML Encryptor Demo</h2>
    
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Output File:</td><td><input type=text name=outputFile value=""></td></tr>
    </table>
    <br/>

    <h3>General Options</h3>
    
    <table>
      <tr><td>Encrypted Data Type:</td><td>
        <div style="display: inline-block; margin: .25em 0;">
          <select name=dataType id="dataType">
            <option value="XMLENCRYPTOR_ENCRYPTEDDATATYPE_ELEMENT">Element</option>
            <option value="XMLENCRYPTOR_ENCRYPTEDDATATYPE_CONTENT">Content</option>
          </select>
        </div>
      </td></tr>
      <tr><td>Encryption method:</td><td>
        <div style="display: inline-block; margin: .25em 0;">
          <select name=encMethod id="encMethod">
            <option value="3DES">3DES</option>
            <option value="AES128">AES128</option>
            <option value="AES192">AES192</option>
            <option value="AES256">AES256</option>
            <option value="Camellia128">Camellia128</option>
            <option value="Camellia192">Camellia192</option>
            <option value="Camellia256">Camellia256</option>
            <option value="DES">DES</option>
            <option value="RC4">RC4</option>
            <option value="SEED">SEED</option>
          </select>
        </div>
      </td></tr>
      <tr><td>XML Node:</td><td><input type=text name=xmlNode value=""></td></tr>
    </table><br/>

    <table>
      <tr><td>Password:</td><td><input type=password name=sPass value=""></td></tr>
    </table><br/><br/>

    <input type="submit" value="Encrypt" />
  </form>
</div><br/>

<?php

function translateEncryptedDataType($dataType){
  switch($dataType){
    case "XMLENCRYPTOR_ENCRYPTEDDATATYPE_ELEMENT":  return XMLENCRYPTOR_ENCRYPTEDDATATYPE_ELEMENT; break;
    case "XMLENCRYPTOR_ENCRYPTEDDATATYPE_CONTENT":  return XMLENCRYPTOR_ENCRYPTEDDATATYPE_CONTENT; break;
    default: return XMLENCRYPTOR_ENCRYPTEDDATATYPE_ELEMENT; break;
  }
}

function getKey($pass, $alg){
  $len = 0;
  
  switch($alg){
    case "AES128":  $len = 16; break;
    case "AES192":  $len = 24; break;
    case "AES256":  $len = 32; break;
    case "Camellia128":  $len = 16; break;
    case "Camellia192":  $len = 24; break;
    case "Camellia256":  $len = 32; break;
    case "DES":  $len = 8; break;
    case "3DES":  $len = 24; break;
    case "RC4":  $len = 16; break;
    case "SEED":  $len = 16; break;
    default:  $len = 0; break;
  }
  
  $res = $pass;
  while (strlen($res) < $len)
    $res = $res . "/" . $pass;

  return $res;
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $xmlencryptor = new SecureBlackbox_XMLEncryptor();

    $xmlencryptor->setUseGCM(FALSE);
    $xmlencryptor->setEncryptKey(FALSE);

    try {
      $xmlencryptor->setInputFile($_REQUEST['inputFile']);
      $xmlencryptor->setOutputFile($_REQUEST['outputFile']);

      $xmlencryptor->setEncryptedDataType(translateEncryptedDataType($_REQUEST['dataType']));
      $xmlencryptor->setEncryptionMethod($_REQUEST['encMethod']);
      $xmlencryptor->setXMLNode($_REQUEST['xmlNode']);

      $xmlencryptor->setEncryptionKey(getKey($_REQUEST['sPass'], $xmlencryptor->getEncryptionMethod()));

      $xmlencryptor->doEncrypt();
      echo "<h2>XML file successfully encrypted</h2>";
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
