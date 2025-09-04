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
require_once('../include/secureblackbox_cadessigner.php');
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
    <h2>CAdES Signing Demo</h2>
    
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
            <option value="BES">BES</option>
            <option value="EPES">EPES</option>
            <option value="T">T</option>
            <option value="C">C</option>
            <option value="XTYPE_1">XType1</option>
            <option value="XTYPE_2">XType2</option>
            <option value="XLTYPE_1">XLType1</option>
            <option value="XLTYPE_2">XLType2</option>
            <option value="BASELINE_B">BaselineB</option>
            <option value="BASELINE_T">BaselineT</option>
            <option value="BASELINE_LT">BaselineLT</option>
            <option value="BASELINE_LTA">BaselineLTA</option>
            <option value="EXTENDED_BES">ExtendedBES</option>
            <option value="EXTENDED_EPES">ExtendedEPES</option>
            <option value="EXTENDED_T">ExtendedT</option>
            <option value="EXTENDED_C">ExtendedC</option>
            <option value="EXTENDED_XTYPE_1">ExtendedXType1</option>
            <option value="EXTENDED_XTYPE_2">ExtendedXType2</option>
            <option value="EXTENDED_XLTYPE_1">ExtendedXLType1</option>
            <option value="EXTENDED_XLTYPE_2">ExtendedXLType2 </option>
            <option value="EXTENDED_A">ExtendedA</option>
            <option value="A">A</option>
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
    <label> <input type="checkbox" name="detached" value="1" size="25"/> Detached </label>
    <br/>
    <br/>

    <b>Signing Certificate</b>
    <table>
      <tr><td>Certificate File:</td><td><input type=text name=sCertFile value=""></td></tr>
      <tr><td>Password:</td><td><input type=password name=sCertPass value=""></td></tr>
    </table>
    <br/>
    <br/>
    
    <b>Timestamp</b><br/><br/>
    <label> <input type="checkbox" name="useTimestamp" value="1" size="25"/> Request a timestamp from TSA server </label>
    <table>
      <tr><td>Timestamp Server:</td><td><input type=text name=timestampServer value=""></td></tr>
    </table>
    <input type="submit" value="Sign" />
  </form>
</div><br/>

<?php

function translateSignatureLevel($sigLevel){
  switch($sigLevel){
    case "BASELINE_B":  return CADESSIGNER_SIGNATURELEVEL_BASELINE_B; break;
    case "BASELINE_T":  return CADESSIGNER_SIGNATURELEVEL_BASELINE_T; break;
    case "BASELINE_LT":  return CADESSIGNER_SIGNATURELEVEL_BASELINE_LT; break;
    case "BASELINE_LTA":  return CADESSIGNER_SIGNATURELEVEL_BASELINE_LTA; break;
    case "BES":  return CADESSIGNER_SIGNATURELEVEL_BES; break;
    case "EPES":  return CADESSIGNER_SIGNATURELEVEL_EPES; break;
    case "T":  return CADESSIGNER_SIGNATURELEVEL_T; break;
    case "C":  return CADESSIGNER_SIGNATURELEVEL_C; break;
    case "XTYPE_1":  return CADESSIGNER_SIGNATURELEVEL_XTYPE_1; break;
    case "XTYPE_2":  return CADESSIGNER_SIGNATURELEVEL_XTYPE_2; break;
    case "XLTYPE_1":  return CADESSIGNER_SIGNATURELEVEL_XLTYPE_1; break;
    case "XLTYPE_2":  return CADESSIGNER_SIGNATURELEVEL_XLTYPE_2; break;
    case "A":  return CADESSIGNER_SIGNATURELEVEL_A; break;
    case "EXTENDED_BES":  return CADESSIGNER_SIGNATURELEVEL_EXTENDED_BES; break;
    case "EXTENDED_EPES":  return CADESSIGNER_SIGNATURELEVEL_EXTENDED_EPES; break;
    case "EXTENDED_T":  return CADESSIGNER_SIGNATURELEVEL_EXTENDED_T; break;
    case "EXTENDED_C":  return CADESSIGNER_SIGNATURELEVEL_EXTENDED_C; break;
    case "EXTENDED_XTYPE_1":  return CADESSIGNER_SIGNATURELEVEL_EXTENDED_XTYPE_1; break;
    case "EXTENDED_XTYPE_2":  return CADESSIGNER_SIGNATURELEVEL_EXTENDED_XTYPE_2; break;
    case "EXTENDED_XLTYPE_1":  return CADESSIGNER_SIGNATURELEVEL_EXTENDED_XLTYPE_1; break;
    case "EXTENDED_XLTYPE_2":  return CADESSIGNER_SIGNATURELEVEL_EXTENDED_XLTYPE_2; break;
    case "EXTENDED_A":  return CADESSIGNER_SIGNATURELEVEL_EXTENDED_A; break;
    default: return CADESSIGNER_SIGNATURELEVEL_UNKNOWN; break;
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $cadessigner = new SecureBlackbox_CAdESSigner();
    $certmgr = new SecureBlackBox_CertificateManager();
    
    try {
      // General options
      $cadessigner->setInputFile($_REQUEST['inputFile']);
      $cadessigner->setOutputFile($_REQUEST['outputFile']);
      
      // Signing options
      $certmgr->doImportFromFile($_REQUEST['sCertFile'], $_REQUEST['sCertPass']);
      $cadessigner->setSigningCertHandle($certmgr->getCertHandle());
      
      $cadessigner->setNewSigLevel(translateSignatureLevel($_REQUEST['sigLevel']));
      $cadessigner->setDetached((isset($_REQUEST['detached']) && (($_REQUEST['detached'] == 'yes') || ($_REQUEST['detached'] == '1'))));
      $hashAlg = $_REQUEST['hashAlg'];
      if (!empty($hashAlg)) {$cadessigner->setHashAlgorithm($hashAlg);}

      if (isset($_REQUEST['useTimestamp']) && (($_REQUEST['useTimestamp'] == 'yes') || ($_REQUEST['useTimestamp'] == '1')))
      {
        $cadessigner->setTimestampServer($_REQUEST['timestampServer']);
      }

      $cadessigner->doSign();
      echo "<h2>Signing Successful</h2>";
    }
    catch (exception $e) {
      echo "<h2>Signing Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>