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
require_once('../include/secureblackbox_messagesigner.php');
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
    <h2>Message Signing Demo</h2>
    
    <h3>General Options</h3>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Output File:</td><td><input type=text name=outputFile value=""></td></tr>
    </table>
    <br/>
    <br/>

    <b>Signing Options</b><br/>
    <table>
      <tr>
        <td>Signature Level:</td>
        <td>
          <select name="sigLevel">
            <option value="MESSAGESIGNER_SIGNATURETYPE_PKCS1DETACHED">Pkcs1 detached</option>
            <option value="MESSAGESIGNER_SIGNATURETYPE_PKCS7DETACHED">Pkcs7 detached</option>
            <option value="MESSAGESIGNER_SIGNATURETYPE_PKCS7ENVELOPING">Pkcs7 enveloping</option>
          </select>
        </td>
      </tr>
      <tr>
        <td>Hash Algorithm:</td>
        <td>
          <select name="hashAlg">
            <option value=""></option>
            <option value="SHA1">SHA1</option>
            <option value="MD5">MD5</option>
            <option value="SHA256">SHA256</option>
            <option value="SHA384">SHA384</option>
            <option value="SHA512">SHA512</option>
            <option value="RIPEMD160">RIPEMD160</option>
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

    <input type="submit" value="Sign" />
  </form>
</div><br/>

<?php

function translateSignatureLevel($sigLevel){
  switch($sigLevel){
    case "MESSAGESIGNER_SIGNATURETYPE_PKCS1DETACHED":  return MESSAGESIGNER_SIGNATURETYPE_PKCS1DETACHED; break;
    case "MESSAGESIGNER_SIGNATURETYPE_PKCS7DETACHED":  return MESSAGESIGNER_SIGNATURETYPE_PKCS7DETACHED; break;
    case "MESSAGESIGNER_SIGNATURETYPE_PKCS7ENVELOPING":  return MESSAGESIGNER_SIGNATURETYPE_PKCS7ENVELOPING; break;
    default: return MESSAGESIGNER_SIGNATURETYPE_UNKNOWN; break;
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $messagesigner = new SecureBlackbox_MessageSigner();
    $certmgr = new SecureBlackBox_CertificateManager();
    
    try {
      // General options
      $messagesigner->setInputFile($_REQUEST['inputFile']);
      $messagesigner->setOutputFile($_REQUEST['outputFile']);
      
      // Signing options
      $certmgr->doImportFromFile($_REQUEST['sCertFile'], $_REQUEST['sCertPass']);
      $messagesigner->setSigningCertHandle($certmgr->getCertHandle());
      
      $messagesigner->setSignatureType(translateSignatureLevel($_REQUEST['sigLevel']));
      $hashAlg = $_REQUEST['hashAlg'];
      if (!empty($hashAlg)) {$messagesigner->setHashAlgorithm($hashAlg);}

      $messagesigner->doSign();
      echo "<h2>Signing Successful</h2>";
    }
    catch (exception $e) {
      echo "<h2>Signing Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>