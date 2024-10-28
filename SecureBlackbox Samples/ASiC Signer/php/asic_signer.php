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
require_once('../include/secureblackbox_asicsigner.php');
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
    <h2>ASiC Signing Demo</h2>
    
    <h3>General Options</h3>
    <table>
      <tr><td>Input Files:</td><td><textarea style="font-family: Arial, sans-serif; width: 100%" name=SourceFiles rows=10>Enter files here, one per line</textarea></td></tr>
      <tr><td>Output File:</td><td><input type=text name=outputFile value=""></td></tr>
    </table>
    <br/>
    <br/>

    <h3>Signing Options</h3>
    <b>Signature Level:</b><br />
    <div style="display: inline-block; margin: .25em 0;">
      <select id="sigLevel" name=sigLevel>
        <option value="BaselineB">Baseline B</option>
        <option value="BaselineT">Baseline T</option>
        <option value="BaselineLT">Baseline LT</option>
        <option value="BaselineLTA">Baseline LTA</option>
        <option value="BES">BES</option>
        <option value="EPES">EPES</option>
        <option value="T">T</option>
        <option value="C">C</option>
        <option value="X">X</option>
        <option value="XType1">XType1</option>
        <option value="XType2">XType2</option>
        <option value="XL">X-L</option>
        <option value="XLType1">XLType1</option>
        <option value="XLType2">XLType2</option>
        <option value="A">A</option>
        <option value="ExtendedBES">Extended BES</option>
        <option value="ExtendedEPES">Extended EPES</option>
        <option value="ExtendedT">Extended T</option>
        <option value="ExtendedC">Extended C</option>
        <option value="ExtendedX">Extended X</option>
        <option value="ExtendedXType1">Extended XType1</option>
        <option value="ExtendedXType2">Extended XType2</option>
        <option value="ExtendedXL">Extended X-L</option>
        <option value="ExtendedXLong ">Extended X-Long</option>
        <option value="ExtendedXLType1">Extended XLType1</option>
        <option value="ExtendedXLType2 ">Extended XLType2</option>
        <option value="ExtendedA">Extended A</option>
      </select>
    </div><br /><br />
    
    <b>Signature Type</b><br/>
    <div style="display: inline-block; margin: .25em 0;">
      <input type=radio id=sigType1 name=sigType value="1" checked />
      <label for=sigType1>CAdES</label>
      &nbsp;&nbsp;&nbsp;
      <input type=radio id=sigType2 name=sigType value="2" />
      <label for=sigType2>XAdES</label>
      &nbsp;&nbsp;&nbsp;
      <input type=radio id=sigType3 name=sigType value="3" />
      <label for=sigType3>Timestamp</label>
      &nbsp;&nbsp;&nbsp;
      <input type=checkbox id="useExtended" name="useExtended" />
      <label for=useExtended>Extended</label>
    </div><br/><br/>
    <b>Signing Certificate</b>
    <table>
      <tr><td>Certificate File:</td><td><input type=text name=sCertFile value=""></td></tr>
      <tr><td>Password:</td><td><input type=password name=sCertPass value=""></td></tr>
    </table>
    
    <h3>Additional Options</h3>
    <b>Policy</b>
    <table>
      <tr><td>Identifier:</td><td><input type=text name=policyIdentifier value=""></td></tr>
      <tr><td>Hash Algorithm:</td><td>
        <select id="policyHashAlgo">
          <option value=""></option>
          <option value="SHA1">SHA1</option>
          <option value="MD5">MD5</option>
          <option value="SHA256">SHA256</option>
          <option value="SHA384">SHA384</option>
          <option value="SHA512">SHA512</option>
          <option value="RIPEMD160">RIPEMD160</option>
        </select>
      </td></tr>
      <tr><td>Hash Value:</td><td><input type=text name=policyHashVal value=""></td></tr>
    </table><br />
    <b>Timestamp</b><br/><br/>
    <input type=checkbox id="useTimestamp" name="useTimestamp" /><label for=useTimestamp>Request a timestamp from TSA server</label>
    <table>
      <tr><td>Timestamp Server:</td><td><input type=text name=timestampServer value=""></td></tr>
    </table>
    <input type="submit" value="Sign" />
  </form>
</div><br/>

<?php

function translateSignatureLevel($sigLevel){
  switch($sigLevel){
    case "BaselineB":  return ASICSIGNER_NEWSIGLEVEL_BASELINE_B; break;
    case "BaselineT":  return ASICSIGNER_NEWSIGLEVEL_BASELINE_T; break;
    case "BaselineLT":  return ASICSIGNER_NEWSIGLEVEL_BASELINE_LT; break;
    case "BaselineLTA":  return ASICSIGNER_NEWSIGLEVEL_BASELINE_LTA; break;
    case "BES":  return ASICSIGNER_NEWSIGLEVEL_BES; break;
    case "EPES":  return ASICSIGNER_NEWSIGLEVEL_EPES; break;
    case "T":  return ASICSIGNER_NEWSIGLEVEL_T; break;
    case "C":  return ASICSIGNER_NEWSIGLEVEL_C; break;
    case "X":  return ASICSIGNER_NEWSIGLEVEL_X; break;
    case "XType1":  return ASICSIGNER_NEWSIGLEVEL_XTYPE_1; break;
    case "XType2":  return ASICSIGNER_NEWSIGLEVEL_XTYPE_2; break;
    case "XL":  return ASICSIGNER_NEWSIGLEVEL_XL; break;
    case "XLType1":  return ASICSIGNER_NEWSIGLEVEL_XLTYPE_1; break;
    case "XLType2":  return ASICSIGNER_NEWSIGLEVEL_XLTYPE_2; break;
    case "A":  return ASICSIGNER_NEWSIGLEVEL_A; break;
    case "ExtendedBES":  return ASICSIGNER_NEWSIGLEVEL_EXTENDED_BES; break;
    case "ExtendedEPES":  return ASICSIGNER_NEWSIGLEVEL_EXTENDED_EPES; break;
    case "ExtendedT":  return ASICSIGNER_NEWSIGLEVEL_EXTENDED_T; break;
    case "ExtendedC":  return ASICSIGNER_NEWSIGLEVEL_EXTENDED_C; break;
    case "ExtendedX":  return ASICSIGNER_NEWSIGLEVEL_EXTENDED_X; break;
    case "ExtendedXType1":  return ASICSIGNER_NEWSIGLEVEL_EXTENDED_XTYPE_1; break;
    case "ExtendedXType2":  return ASICSIGNER_NEWSIGLEVEL_EXTENDED_XTYPE_2; break;
    case "ExtendedXL":  return ASICSIGNER_NEWSIGLEVEL_EXTENDED_XL; break;
    case "ExtendedXLong":  return ASICSIGNER_NEWSIGLEVEL_EXTENDED_XLONG; break;
    case "ExtendedXLType1":  return ASICSIGNER_NEWSIGLEVEL_EXTENDED_XLTYPE_1; break;
    case "ExtendedXLType2":  return ASICSIGNER_NEWSIGLEVEL_EXTENDED_XLTYPE_2; break;
    case "ExtendedA":  return ASICSIGNER_NEWSIGLEVEL_EXTENDED_A; break;
    default: return ASICSIGNER_NEWSIGLEVEL_UNKNOWN; break;
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $asicsigner = new SecureBlackbox_ASiCSigner();
    $certmgr = new SecureBlackBox_CertificateManager();
    
    try {
      // General options
      $SourceFiles = str_replace("\r\n", ",", $_REQUEST['SourceFiles']);
      $asicsigner->setSourceFiles($SourceFiles);
      $asicsigner->setOutputFile($_REQUEST['outputFile']);

      // Signing options
      try {
        $certmgr->doImportFromFile($_REQUEST['sCertFile'], $_REQUEST['sCertPass']);
      } catch (exception $e) {
        echo "<h2>Failed to load certificate</h2><p>" . $e->getMessage() . "</p>";
        return;
      }
      $asicsigner->setSigningCertHandle($certmgr->getCertHandle());

      // Additional options
      $asicsigner->setExtended(!empty($_REQUEST['useExtended']));
      $asicsigner->setNewSigSignatureType($_REQUEST['sigType']);
      $asicsigner->setNewSigLevel(translateSignatureLevel($_REQUEST['sigLevel']));
      $asicsigner->setNewSigPolicyID($_REQUEST['policyIdentifier']);
      $asicsigner->setNewSigPolicyHashAlgorithm(!empty($_REQUEST['policyHashAlgo']));
      $asicsigner->setNewSigPolicyHash($_REQUEST['policyHashVal']);
      $asicsigner->setTimestampServer($_REQUEST['timestampServer']);
    
      $asicsigner->doSign();
      echo "<h2>Signing Successful</h2>";
    }
    catch (exception $e) {
      echo "<h2>Signing Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>