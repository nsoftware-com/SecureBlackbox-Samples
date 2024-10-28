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
require_once('../include/secureblackbox_hashfunction.php');
require_once('../include/secureblackbox_cryptokeymanager.php');
require_once('../include/secureblackbox_symmetriccrypto.php');
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
    <h2>Hash function Demo</h2>
    
    <b>General Options</b><br/><br/>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Password:</td><td><input type=password name=sPass value=""></td></tr>
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

    <br/><br/><br/>
    <input type="submit" value="Hash" />
  </form>
</div><br/>

<?php
  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $hashfunction = new SecureBlackbox_HashFunction();
    $keymgr = new SecureBlackBox_CryptoKeyManager();

    try 
    {
      // Key from password
      $pass = trim($_REQUEST['sPass']);
      if (strlen($pass) > 0)
      {
        $keymgr->doDeriveKey(256, $pass, '');
        
        $hashfunction->setKeyHandle($keymgr->getKeyHandle());
      }

      switch(trim($_REQUEST['encoding'])){
        case "Binary":  $hashfunction->setOutputEncoding(HASHFUNCTION_OUTPUTENCODING_BINARY); break;
        case "Base64":  $hashfunction->setOutputEncoding(HASHFUNCTION_OUTPUTENCODING_BASE_64); break;
        case "Compact":  $hashfunction->setOutputEncoding(HASHFUNCTION_OUTPUTENCODING_COMPACT); break;
        case "JSON":  $hashfunction->setOutputEncoding(HASHFUNCTION_OUTPUTENCODING_JSON); break;
        default: $hashfunction->setOutputEncoding(HASHFUNCTION_OUTPUTENCODING_DEFAULT); break;
      }

      // Hash
      $res = $hashfunction->doHashFile($_REQUEST['inputFile']);

      echo "<h2>Hash value: " . $res . "</h2>";
    }
    catch (exception $e) {
      echo "<h2>Hashing Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>