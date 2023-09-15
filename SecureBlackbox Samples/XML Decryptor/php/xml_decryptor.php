<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - XML Decryptor</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - XML Decryptor">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>XML Decryptor</h2>
<p>A tiny XML decryption example.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/secureblackbox_xmldecryptor.php');
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
    <h2>XML Decryptor Demo</h2>
    
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Output File:</td><td><input type=text name=outputFile value=""></td></tr>
    </table>
    <br/>

    <table>
      <tr><td>Password:</td><td><input type=password name=sPass value=""></td></tr>
    </table><br/><br/>

    <input type="submit" value="Decrypt" />
  </form>
</div><br/>

<?php
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
  
class MyXMLDecryptor extends SecureBlackbox_XMLDecryptor
{
  private $pass;
  
  function setPassword($value){
    $this->pass = $value;

    return $this->pass;
  }
  
  function fireDecryptionInfoNeeded($param){
    $this->setDecryptionKey(getKey($this->pass, $this->getEncryptionMethod()));

    return $param;
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $xmldecryptor = new MyXMLDecryptor();

    try {
      $xmldecryptor->setInputFile($_REQUEST['inputFile']);
      $xmldecryptor->setOutputFile($_REQUEST['outputFile']);

      $xmldecryptor->setPassword($_REQUEST['sPass']);

      $xmldecryptor->doDecrypt();
      echo "<h2>XML file successfully decrypted</h2>";
    }
    catch (exception $e) {
      echo "<h2>Decryption Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
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
