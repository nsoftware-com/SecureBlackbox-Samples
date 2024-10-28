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
require_once('../include/secureblackbox_messageencryptor.php');
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
    <h2>Message Encryptor Demo</h2>
    
    <h3>General Options</h3>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Output File:</td><td><input type=text name=outputFile value=""></td></tr>
    </table>
    <br/>
    <br/>

    <b>Encrypting Options</b><br/>
    <table>
      <tr>
        <td>Encryption algorithm::</td>
        <td>
          <select name="encAlg">
            <option value="3DES">3DES</option>
            <option value="RC4">RC4</option>
            <option value="RC2">RC2</option>
            <option value="AES128">AES128</option>
            <option value="AES192">AES192</option>
            <option value="AES256">AES256</option>
            <option value="Twofish128">Twofish128</option>
          </select>
        </td>
      </tr>
    </table>
    <br/>
    <br/>

    <b>Encryption Certificate</b>
    <table>
      <tr><td>Certificate File:</td><td><input type=text name=sCertFile value=""></td></tr>
      <tr><td>Password:</td><td><input type=password name=sCertPass value=""></td></tr>
    </table>
    <br/>
    <br/>
    

    <input type="submit" value="Encrypt" />
  </form>
</div><br/>

<?php
  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $messageencryptor = new SecureBlackbox_MessageEncryptor();
    $certmgr = new SecureBlackBox_CertificateManager();
    
    try {
      // General options
      $messageencryptor->setInputFile($_REQUEST['inputFile']);
      $messageencryptor->setOutputFile($_REQUEST['outputFile']);
      
      // Encryption options
      $certmgr->doImportFromFile($_REQUEST['sCertFile'], $_REQUEST['sCertPass']);
      $messageencryptor->setEncryptionCertCount(1);
      $messageencryptor->setEncryptionCertHandle(0, $certmgr->getCertHandle());

      $messageencryptor->setEncryptionAlgorithm($_REQUEST['encAlg']);

      $messageencryptor->doEncrypt();
      echo "<h2>The file successfully encrypted</h2>";
    }
    catch (exception $e) {
      echo "<h2>Encryption Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>