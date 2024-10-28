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