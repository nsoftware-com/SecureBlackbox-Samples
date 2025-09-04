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
require_once('../include/secureblackbox_pdfsigner.php');
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
    <h2>PDF Signing Demo</h2>
    
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
        <td>Level:</td>
        <td>
          <select name="level">
            <option value="PDFSIGNER_SIGNATURELEVEL_LEGACY">Legacy</option>
            <option value="PDFSIGNER_SIGNATURELEVEL_BES">BES</option>
            <option value="PDFSIGNER_SIGNATURELEVEL_EPES">EPES</option>
            <option value="PDFSIGNER_SIGNATURELEVEL_LTV">LTV</option>
            <option value="PDFSIGNER_SIGNATURELEVEL_DOCUMENT_TIMESTAMP">DocumentTimestamp</option>
          </select>
        </td>
      </tr>
    </table>
    <label> <input type="checkbox" name="visible" value="1" size="25"/> Visible signature </label>
    <br/>
    <br/>

    <b>Signing Certificate</b>
    <table>
      <tr><td>Certificate File:</td><td><input type=text name=sCertFile value=""></td></tr>
      <tr><td>Password:</td><td><input type=password name=sCertPass value=""></td></tr>
    </table>
    <br/>
    <br/>

    <input type=checkbox name="autocollectmissrevInfo" /><label for=autocollectmissrevInfo>Automatically collect missing revocation information</label>
    <input type=checkbox name="ignoreChainValidationErrors" /><label for=ignoreChainValidationErrors>Ignore chain validation errors</label>
    <input type=checkbox name="forceCompleteChainValidation" /><label for=forceCompleteChainValidation>Force complete chain validation</label>
    <input type=checkbox name="deepValidation" /><label for=deepValidation>Deep validation</label>
    <br/>
    <br/>
    
    <b>Timestamp</b><br/><br/>
    <label> <input type="checkbox" name="useTimestamp" value="1" size="25"/> Request a timestamp from TSA server </label>
    <table>
      <tr><td>Timestamp Server:</td><td><input type=text name=timestampServer value=""></td></tr>
    </table>
    <br/>
    <br/>

    <input type="submit" value="Sign" />
  </form>
</div><br/>

<?php

function translatePDFLevel($level){
  switch($level){
    case "PDFSIGNER_SIGNATURELEVEL_LEGACY":  return PDFSIGNER_SIGNATURELEVEL_LEGACY; break;
    case "PDFSIGNER_SIGNATURELEVEL_BES":  return PDFSIGNER_SIGNATURELEVEL_BES; break;
    case "PDFSIGNER_SIGNATURELEVEL_EPES":  return PDFSIGNER_SIGNATURELEVEL_EPES; break;
    case "PDFSIGNER_SIGNATURELEVEL_LTV":  return PDFSIGNER_SIGNATURELEVEL_LTV; break;
    case "PDFSIGNER_SIGNATURELEVEL_DOCUMENT_TIMESTAMP":  return PDFSIGNER_SIGNATURELEVEL_DOCUMENT_TIMESTAMP; break;
    default: return PDFSIGNER_SIGNATURELEVEL_LEGACY; break;
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $pdfsigner = new SecureBlackbox_PDFSigner();
    $certmgr = new SecureBlackBox_CertificateManager();
    
    try {
      // General options
      $pdfsigner->setInputFile($_REQUEST['inputFile']);
      $pdfsigner->setOutputFile($_REQUEST['outputFile']);
      
      // Signing options
      try {
        $certmgr->doImportFromFile($_REQUEST['sCertFile'], $_REQUEST['sCertPass']);
      } catch (exception $e) {
        echo "<h2>Failed to load certificate</h2><p>" . $e->getMessage() . "</p>";
        return;
      }
      $pdfsigner->setSigningCertHandle($certmgr->getCertHandle());
      
      $pdfsigner->setNewSigLevel(translatePDFLevel($_REQUEST['level']));
      $visible = (isset($_REQUEST['visible']) && (($_REQUEST['visible'] == 'yes') || ($_REQUEST['visible'] == '1')));
      $pdfsigner->setWidgetInvisible(!$visible);

      $pdfsigner->doConfig("AutoCollectRevocationInfo=" . (!empty($_REQUEST['autocollectmissrevInfo']) ? "True" : "False"));
      $pdfsigner->setIgnoreChainValidationErrors(!empty($_REQUEST['ignoreChainValidationErrors']));
      $pdfsigner->doConfig("ForceCompleteChainValidation=" . (!empty($_REQUEST['forceCompleteChainValidation']) ? "True" : "False"));
      $pdfsigner->doConfig("DeepValidation=" . (!empty($_REQUEST['deepValidation']) ? "True" : "False"));

      if (isset($_REQUEST['useTimestamp']) && (($_REQUEST['useTimestamp'] == 'yes') || ($_REQUEST['useTimestamp'] == '1')))
      {
        $pdfsigner->setTimestampServer($_REQUEST['timestampServer']);
      }

      $pdfsigner->doSign();
      echo "<h2>PDF file successfully signed</h2>";
    }
    catch (exception $e) {
      echo "<h2>Signing Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>