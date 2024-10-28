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
      <tr><td>Input:</td><td><input type=text name=inputData value=""></td></tr>
      <tr><td>Signature (for verifying):</td><td><input type=text name=signatureData value=""></td></tr>
    </table>
    <br/>
    <br/>

    <label> <input type="checkbox" name="compact" value="1" size="25"/> Compact </label>
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
      $inputData = $_REQUEST['inputData'];
      $signatureData = $_REQUEST['signatureData'];
      $compact = (isset($_REQUEST['compact']) && (($_REQUEST['compact'] == 'yes') || ($_REQUEST['compact'] == '1')));

      // Signing options
      $certmgr->doImportFromFile($_REQUEST['sCertFile'], $_REQUEST['sCertPass']);
      $keymgr->setCertHandle($certmgr->getCertHandle());
      $keymgr->doImportFromCert();
      $crypto->setKeyHandle($keymgr->getKeyHandle());

      $comtype = $_POST["comtype"];

      if ($comtype == "Sign")
      {
        if ($compact)
        {
          $crypto->setOutputEncoding(PUBLICKEYCRYPTO_OUTPUTENCODING_COMPACT);
        }
        else
        {
          $crypto->setOutputEncoding(PUBLICKEYCRYPTO_OUTPUTENCODING_JSON);
        }

        $signatureData = $crypto->doSign($inputData, TRUE);
        echo "<h2>Signature string: " . $signatureData . "</h2>";
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

        $crypto->doVerifyDetached($inputData, $signatureData);

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