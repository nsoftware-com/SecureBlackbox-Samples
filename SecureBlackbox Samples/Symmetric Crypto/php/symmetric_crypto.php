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
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Output:</td><td><input type=text name=outputFile value=""></td></tr>
    </table>
    <br/>
    <br/>

    <table>
      <tr>
        <td>Encoding:</td>
        <td>
          <select name="encoding">
            <option value="Binary">Binary</option>
            <option selected="selected" value="Base64">Base64</option>
            <option value="Compact">Compact</option>
            <option value="JSON">JSON</option>
          </select>
        </td>
      </tr>
    </table>
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
      $inputFile = $_REQUEST['inputFile'];
      $outputFile = $_REQUEST['outputFile'];

      $keyBits = 256;

      $comtype = $_POST["comtype"];
      if ($comtype == "Encrypt")
      {
        switch(trim($_REQUEST['encoding'])){
          case "Binary":  $crypto->setOutputEncoding(PUBLICKEYCRYPTO_OUTPUTENCODING_BINARY); break;
          case "Base64":  $crypto->setOutputEncoding(PUBLICKEYCRYPTO_OUTPUTENCODING_BASE_64); break;
          case "Compact":  $crypto->setOutputEncoding(PUBLICKEYCRYPTO_OUTPUTENCODING_COMPACT); break;
          case "JSON":  $crypto->setOutputEncoding(PUBLICKEYCRYPTO_OUTPUTENCODING_JSON); $keyBits = 256; break;
          default: $crypto->setOutputEncoding(PUBLICKEYCRYPTO_OUTPUTENCODING_DEFAULT); $keyBits = 256; break;
        }
      }
      else
      {
        switch(trim($_REQUEST['encoding'])){
          case "Binary":  $crypto->setInputEncoding(PUBLICKEYCRYPTO_INPUTENCODING_BINARY); break;
          case "Base64":  $crypto->setInputEncoding(PUBLICKEYCRYPTO_INPUTENCODING_BASE_64); break;
          case "Compact":  $crypto->setInputEncoding(PUBLICKEYCRYPTO_INPUTENCODING_COMPACT); break;
          case "JSON":  $crypto->setInputEncoding(PUBLICKEYCRYPTO_INPUTENCODING_JSON); $keyBits = 256; break;
          default: $crypto->setInputEncoding(PUBLICKEYCRYPTO_INPUTENCODING_DEFAULT); $keyBits = 256; break;
        }
      }

      // Key from password
      $pass = trim($_REQUEST['sPass']);
      $keymgr->doDeriveKey($keyBits, $pass, '');
      $keymgr->setKeyIV("                ");
      
      $crypto->setKeyHandle($keymgr->getKeyHandle());

      if ($comtype == "Encrypt")
      {
        $crypto->doEncryptFile($inputFile, $outputFile);
        echo "<h2>The file was encrypted successfully</h2>";
      }
      else
      {
        $crypto->doDecryptFile($inputFile, $outputFile);
        echo "<h2>The file was decrypted successfully</h2>";
      }
    }
    catch (exception $e) {
      echo "<h2>Encryption/decryption Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>