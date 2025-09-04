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
require_once('../include/secureblackbox_jadessigner.php');
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
    <h2>JAdES Signing Demo</h2>
    
    <h3>General Options</h3>
    <table>
      <tr><td>Data File:</td><td><input type=text name=dataFile value=""></td></tr>
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
            <option value="JWS">JWS</option>
            <option value="BASELINE_B" selected>Baseline-B</option>
            <option value="BASELINE_T">Baseline-T</option>
            <option value="BASELINE_LT">Baseline-LT</option>
            <option value="BASELINE_LTA">Baseline-LTA</option>
          </select>
        </td>
      </tr>
    </table>
    <label> <input type="checkbox" name="detached" value="1" size="25"/> Detached </label>
    <label> <input type="checkbox" name="compact" value="1" size="25"/> Compact Form </label>
    <label> <input type="checkbox" name="flattened" value="1" size="25" checked/> Flattened Signature </label>
    <br/>
    <br/>

    <b>Signing Certificate</b>
    <table>
      <tr><td>Certificate File:</td><td><input type=text name=sCertFile value=""></td></tr>
      <tr><td>Password:</td><td><input type=password name=sCertPass value=""></td></tr>
    </table>
    <br/>
    <br/>

    <input type=checkbox name="ignoreChainValidationErrors" /><label for=ignoreChainValidationErrors>Ignore chain validation errors</label>
    <input type=checkbox name="forceCompleteChainValidation" /><label for=forceCompleteChainValidation>Force complete chain validation</label>
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

function translateJAdESLevel($level){
  switch($level){
    case "JWS":  return JADESSIGNER_NEWSIGLEVEL_GENERIC; break;
    case "BASELINE_B":  return JADESSIGNER_NEWSIGLEVEL_BASELINE_B; break;
    case "BASELINE_T":  return JADESSIGNER_NEWSIGLEVEL_BASELINE_T; break;
    case "BASELINE_LT":  return JADESSIGNER_NEWSIGLEVEL_BASELINE_LT; break;
    case "BASELINE_LTA":  return JADESSIGNER_NEWSIGLEVEL_BASELINE_LTA; break;
    default: return JADESSIGNER_SIGNATURELEVEL_BASELINE_B; break;
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $jadessigner = new SecureBlackbox_JAdESSigner();
    $certmgr = new SecureBlackBox_CertificateManager();
    
    try {
      // General options
      $jadessigner->setDataFile($_REQUEST['dataFile']);
      $jadessigner->setOutputFile($_REQUEST['outputFile']);
      
      // Signing options
      try {
        $certmgr->doImportFromFile($_REQUEST['sCertFile'], $_REQUEST['sCertPass']);
      } catch (exception $e) {
        echo "<h2>Failed to load certificate</h2><p>" . $e->getMessage() . "</p>";
        return;
      }
      $jadessigner->setSigningCertHandle($certmgr->getCertHandle());
      
      $jadessigner->setNewSigLevel(translateJAdESLevel($_REQUEST['level']));

      $detached = (isset($_REQUEST['detached']) && (($_REQUEST['detached'] == 'yes') || ($_REQUEST['detached'] == '1')));
      $jadessigner->setDetached($detached);

      $compact = (isset($_REQUEST['compact']) && (($_REQUEST['compact'] == 'yes') || ($_REQUEST['compact'] == '1')));
      $jadessigner->setCompactForm($compact);

      $flattened = (isset($_REQUEST['flattened']) && (($_REQUEST['flattened'] == 'yes') || ($_REQUEST['flattened'] == '1')));
      $jadessigner->setFlattenedSignature($flattened);

      $jadessigner->setIgnoreChainValidationErrors(!empty($_REQUEST['ignoreChainValidationErrors']));
      $jadessigner->doConfig("ForceCompleteChainValidation=" . (!empty($_REQUEST['forceCompleteChainValidation']) ? "True" : "False"));

      if (isset($_REQUEST['useTimestamp']) && (($_REQUEST['useTimestamp'] == 'yes') || ($_REQUEST['useTimestamp'] == '1')))
      {
        if ($jadessigner->getNewSigLevel() < JADESSIGNER_SIGNATURELEVEL_BASELINE_T)
          $jadessigner->setNewSigLevel(JADESSIGNER_SIGNATURELEVEL_BASELINE_T);

        $jadessigner->setTimestampServer($_REQUEST['timestampServer']);
      }

      $jadessigner->doSign();
      echo "<h2>JWS/JAdES signature successfully created</h2>";
      echo "<h3>" . $jadessigner->getOutputString() . "</h3>"; // in case if OutputFile is not set
    }
    catch (exception $e) {
      echo "<h2>Signing Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>