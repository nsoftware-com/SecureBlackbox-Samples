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
      $keymgr->setKeyIV("                ");
      
      $crypto->setKeyHandle($keymgr->getKeyHandle());

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