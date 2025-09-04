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
require_once('../include/secureblackbox_officesigner.php');
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
    <h2>Office Signing Demo</h2>
    
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
        <td>Signature Type:</td>
        <td>
          <select name="sigType">
            <option value="DEFAULT" selected>Default</option>
            <option value="BINARY_CRYPTO_API">BinaryCryptoAPI</option>
            <option value="BINARY_XML">BinaryXML</option>
            <option value="OPEN_XML">OpenXML</option>
            <option value="OPEN_XPS">OpenXPS</option>
            <option value="OPEN_DOCUMENT">OpenOffice</option>
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
            <option value="SHA256" selected>SHA256</option>
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

    <b>Additional options</b><br/>
    <label> <input type="checkbox" name="signDocument" value="1" size="25"/> Sign Document </label>
    <br/>
    <label> <input type="checkbox" name="signsignatureOrigin" value="1" size="25"/> Sign Signature Origin </label>
    <br/>
    <label> <input type="checkbox" name="signcoreProperties" value="1" size="25"/> Sign Core Properties </label>
    <br/>
    <br/>

    <input type="submit" value="Sign" />
  </form>
</div><br/>

<?php

function translateSignatureType($sigType){
  switch($sigType){
    case "BINARY_CRYPTO_API":  return OFFICESIGNER_NEWSIGSIGNATURETYPE_BINARY_CRYPTO_API; break;
    case "BINARY_XML":  return OFFICESIGNER_NEWSIGSIGNATURETYPE_BINARY_XML; break;
    case "OPEN_XML":  return OFFICESIGNER_NEWSIGSIGNATURETYPE_OPEN_XML; break;
    case "OPEN_XPS":  return OFFICESIGNER_NEWSIGSIGNATURETYPE_OPEN_XPS; break;
    case "OPEN_DOCUMENT":  return OFFICESIGNER_NEWSIGSIGNATURETYPE_OPEN_DOCUMENT; break;
    default: return OFFICESIGNER_NEWSIGSIGNATURETYPE_DEFAULT; break;
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $officesigner = new SecureBlackbox_OfficeSigner();
    $certmgr = new SecureBlackBox_CertificateManager();
    
    try {
      // General options
      $officesigner->setInputFile($_REQUEST['inputFile']);
      $officesigner->setOutputFile($_REQUEST['outputFile']);
      
      // Signing options
      try {
        $certmgr->doImportFromFile($_REQUEST['sCertFile'], $_REQUEST['sCertPass']);
      } catch (exception $e) {
        echo "<h2>Failed to load certificate</h2><p>" . $e->getMessage() . "</p>";
        return;
      }
      $officesigner->setSigningCertHandle($certmgr->getCertHandle());
      
      $officesigner->setNewSigSignatureType(translateSignatureType($_REQUEST['sigType']));
      $hashAlg = $_REQUEST['hashAlg'];
      if (!empty($hashAlg)) {$officesigner->setNewSigHashAlgorithm($hashAlg);}

      // Additional options
      $signDocument = (isset($_REQUEST['signDocument']) && (($_REQUEST['signDocument'] == 'yes') || ($_REQUEST['signDocument'] == '1')));
      $officesigner->setNewSigDocumentSigned($signDocument);

      $signsignatureOrigin = (isset($_REQUEST['signsignatureOrigin']) && (($_REQUEST['signsignatureOrigin'] == 'yes') || ($_REQUEST['signsignatureOrigin'] == '1')));
      $officesigner->setNewSigSignatureOriginSigned($signsignatureOrigin);

      $signcoreProperties = (isset($_REQUEST['signcoreProperties']) && (($_REQUEST['signcoreProperties'] == 'yes') || ($_REQUEST['signcoreProperties'] == '1')));
      $officesigner->setNewSigCorePropertiesSigned($signcoreProperties);

      $officesigner->doSign();
      echo "<h2>Office file successfully signed</h2>";
    }
    catch (exception $e) {
      echo "<h2>Signing Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>