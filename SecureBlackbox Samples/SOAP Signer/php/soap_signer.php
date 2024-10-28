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
require_once('../include/secureblackbox_soapsigner.php');
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
    <h2>SOAP Signing Demo</h2>
    
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
            <option value="WSSSIGNATURE" selected>WSS Signature</option>
            <option value="SOAPSIGNATURE">SOAP Signature</option>
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
    <label> <input type="checkbox" name="signBody" value="1" size="25"/> Sign Body </label>
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

function translateSignatureType($sigType){
  switch($sigType){
    case "WSSSIGNATURE":  return SOAPSIGNER_NEWSIGSIGNATURETYPE_WSSSIGNATURE; break;
    case "SOAPSIGNATURE":  return SOAPSIGNER_NEWSIGSIGNATURETYPE_SOAPSIGNATURE; break;
    default: return SOAPSIGNER_NEWSIGSIGNATURETYPE_UNKNOWN; break;
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $soapsigner = new SecureBlackbox_SOAPSigner();
    $certmgr = new SecureBlackBox_CertificateManager();
    
    try {
      // General options
      $soapsigner->setInputFile($_REQUEST['inputFile']);
      $soapsigner->setOutputFile($_REQUEST['outputFile']);
      
      // Signing options
      try {
        $certmgr->doImportFromFile($_REQUEST['sCertFile'], $_REQUEST['sCertPass']);
      } catch (exception $e) {
        echo "<h2>Failed to load certificate</h2><p>" . $e->getMessage() . "</p>";
        return;
      }
      $soapsigner->setSigningCertHandle($certmgr->getCertHandle());
      
      $soapsigner->setNewSigSignatureType(translateSignatureType($_REQUEST['sigType']));
      $hashAlg = $_REQUEST['hashAlg'];
      if (!empty($hashAlg)) {$soapsigner->setNewSigHashAlgorithm($hashAlg);}
      if (isset($_REQUEST['signBody']) && (($_REQUEST['signBody'] == 'yes') || ($_REQUEST['signBody'] == '1')))
      {
        $soapsigner->doAddBodyReference("", TRUE);
      }

      $soapsigner->doSign();
      echo "<h2>SOAP message successfully signed</h2>";
    }
    catch (exception $e) {
      echo "<h2>Signing Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>