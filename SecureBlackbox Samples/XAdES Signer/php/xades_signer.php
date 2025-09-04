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
require_once('../include/secureblackbox_xadessigner.php');
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
    <h2>XAdES Signing Demo</h2>
    
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Output File:</td><td><input type=text name=outputFile value=""></td></tr>
    </table>
    <br/>

    <h3>General Options</h3>
    
    <table>
      <tr><td>Canonicalization Method:</td><td>
        <div style="display: inline-block; margin: .25em 0;">
          <select name=canonMethod id="canonMethod">
            <option value="0">None</option>	
            <option value="1" selected>Canon</option>	
            <option value="2">CanonComment</option>	
            <option value="3">ExclCanon</option>	
            <option value="4">ExclCanonComment</option>	
            <option value="5">MinCanon</option>	
            <option value="6">Canon_v1_1</option>	
            <option value="7">CanonComment_v1_1</option>
          </select>
        </div>
      </td></tr>
      <tr><td>Hash Algorithm:</td><td>
        <div style="display: inline-block; margin: .25em 0;">
          <select name=hashAlgo id="hashAlgo">
            <option value="MD5">MD5</option>
            <option value="SHA1">SHA1</option>
            <option value="SHA224">SHA224</option>
            <option value="SHA256" selected>SHA256</option>
            <option value="SHA384">SHA384</option>
            <option value="SHA512">SHA512</option>
            <option value="RIPEMD160">RIPEMD160</option>
            <option value="GOST_R3411_1994">GOST1994</option>
            <option value="WHIRLPOOL">WHIRLPOOL</option>
            <option value="SHA3_256">SHA3_256</option>
            <option value="SHA3_384">SHA3_384</option>
            <option value="SHA3_512">SHA3_512</option>
          </select>
        </div>
      </td></tr>
    </table><br />

    <input type=checkbox id="detached" name="detached" /><label for=detached>Detached</label><br /><br />

    <h3>XAdES Options</h3>

    <table>
      <tr><td>Version:</td><td>
        <div style="display: inline-block; margin: .25em 0;">
          <select name=xadesVersion id="xadesVersion">
            <option value="1">XAdES v1.1.1</option>
            <option value="2">XAdES v1.2.2</option>
            <option value="3" selected>XAdES v1.3.2</option>
            <option value="4">XAdES v1.4.1 (aka v1.4.2)</option>
          </select>
        </div>
      </td></tr>
      <tr><td>Level/Form:</td><td>
        <div style="display: inline-block; margin: .25em 0;">
          <select name=sigLevel id="sigLevel">
            <option value="BaselineB">Baseline B</option>
            <option value="BaselineT">Baseline T</option>
            <option value="BaselineLT">Baseline LT</option>
            <option value="BaselineLTA">Baseline LTA</option>
            <option value="BES">BES</option>
            <option value="EPES">EPES</option>
            <option value="T">T</option>
            <option value="C">C</option>
            <option value="X">X</option>
            <option value="XL">X-L</option>
            <option value="A">A</option>
            <option value="ExtendedBES">Extended BES</option>
            <option value="ExtendedEPES">Extended EPES</option>
            <option value="ExtendedT">Extended T</option>
            <option value="ExtendedC">Extended C</option>
            <option value="ExtendedX">Extended X</option>
            <option value="ExtendedXL">Extended X-L</option>
            <option value="ExtendedXLong ">Extended X-Long</option>
            <option value="ExtendedA">Extended A</option>
          </select>
        </div>
      </td></tr>
      <tr><td>Timestamp Server:</td><td><input type=text name=timestampServer value=""></td></tr>
    </table>
    
    <h3>Key Options</h3><br />
    <p>Enter the path(s) for any certificate(s), one per line. If a password is required add a semicolon followed by the password (e.g. C:\path\to\my.pfx;password).</p>
    <table>
      <tr><td>Signing Certificates:</td><td><textarea style="font-family: Arial, sans-serif; width: 100%" name=signingCerts rows=10></textarea></td></tr>
    </table><br /><br />
    
    <input type=checkbox id="includeKey" name="includeKey" /><label for=includeKey>Include Key (public part)</label>
    <table>
      <tr><td>Key Name:</td><td><input type=text name=keyName value=""></td></tr>
    </table><br />
    
    <input type="submit" value="Sign" />
  </form>
</div><br/>

<?php

function translateSignatureLevel($sigLevel){
  switch($sigLevel){
    case "BaselineB":  return XADESSIGNER_NEWSIGLEVEL_BASELINE_B; break;
    case "BaselineT":  return XADESSIGNER_NEWSIGLEVEL_BASELINE_T; break;
    case "BaselineLT":  return XADESSIGNER_NEWSIGLEVEL_BASELINE_LT; break;
    case "BaselineLTA":  return XADESSIGNER_NEWSIGLEVEL_BASELINE_LTA; break;
    case "BES":  return XADESSIGNER_NEWSIGLEVEL_BES; break;
    case "EPES":  return XADESSIGNER_NEWSIGLEVEL_EPES; break;
    case "T":  return XADESSIGNER_NEWSIGLEVEL_T; break;
    case "C":  return XADESSIGNER_NEWSIGLEVEL_C; break;
    case "X":  return XADESSIGNER_NEWSIGLEVEL_X; break;
    case "XType1":  return XADESSIGNER_NEWSIGLEVEL_XTYPE_1; break;
    case "XType2":  return XADESSIGNER_NEWSIGLEVEL_XTYPE_2; break;
    case "XL":  return XADESSIGNER_NEWSIGLEVEL_XL; break;
    case "XLType1":  return XADESSIGNER_NEWSIGLEVEL_XLTYPE_1; break;
    case "XLType2":  return XADESSIGNER_NEWSIGLEVEL_XLTYPE_2; break;
    case "A":  return XADESSIGNER_NEWSIGLEVEL_A; break;
    case "ExtendedBES":  return XADESSIGNER_NEWSIGLEVEL_EXTENDED_BES; break;
    case "ExtendedEPES":  return XADESSIGNER_NEWSIGLEVEL_EXTENDED_EPES; break;
    case "ExtendedT":  return XADESSIGNER_NEWSIGLEVEL_EXTENDED_T; break;
    case "ExtendedC":  return XADESSIGNER_NEWSIGLEVEL_EXTENDED_C; break;
    case "ExtendedX":  return XADESSIGNER_NEWSIGLEVEL_EXTENDED_X; break;
    case "ExtendedXType1":  return XADESSIGNER_NEWSIGLEVEL_EXTENDED_XTYPE_1; break;
    case "ExtendedXType2":  return XADESSIGNER_NEWSIGLEVEL_EXTENDED_XTYPE_2; break;
    case "ExtendedXL":  return XADESSIGNER_NEWSIGLEVEL_EXTENDED_XL; break;
    case "ExtendedXLong":  return XADESSIGNER_NEWSIGLEVEL_EXTENDED_XLONG; break;
    case "ExtendedXLType1":  return XADESSIGNER_NEWSIGLEVEL_EXTENDED_XLTYPE_1; break;
    case "ExtendedXLType2":  return XADESSIGNER_NEWSIGLEVEL_EXTENDED_XLTYPE_2; break;
    case "ExtendedA":  return XADESSIGNER_NEWSIGLEVEL_EXTENDED_A; break;
    default: return XADESSIGNER_NEWSIGLEVEL_UNKNOWN; break;
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $xadessigner = new SecureBlackbox_XAdESSigner();
    $certmgr = new SecureBlackBox_CertificateManager();
    
    try {
      $xadessigner->setOutputFile($_REQUEST['outputFile']);
      
      if(!empty($_REQUEST['detached'])){
        $xadessigner->setDataFile($_REQUEST['inputFile']);
        $xadessigner->setDataType(XADESSIGNER_DATATYPE_BINARY);
        $xadessigner->setDataURI(basename($_REQUEST['inputFile']));
        $xadessigner->setNewSigSignatureType(XADESSIGNER_NEWSIGSIGNATURETYPE_DETACHED);
      } else {
        $xadessigner->setInputFile($_REQUEST['inputFile']);
        $xadessigner->setNewSigSignatureType(XADESSIGNER_NEWSIGSIGNATURETYPE_ENVELOPED);
      }

      $xadessigner->setNewSigCanonicalizationMethod($_REQUEST['canonMethod']);
      $xadessigner->setNewSigHashAlgorithm($_REQUEST['hashAlgo']);
      $xadessigner->setNewSigXAdESVersion($_REQUEST['xadesVersion']);
      $xadessigner->setNewSigLevel(translateSignatureLevel($_REQUEST['sigLevel']));
      $xadessigner->setTimestampServer($_REQUEST['timestampServer']);
      
      $xadessigner->doConfig("IncludeKey=" . (!empty($_REQUEST['includeKey']) ? "true" : "false" ));
      $xadessigner->doConfig("KeyName=" . $_REQUEST['keyName']);
    
      $signingCerts = explode("\r\n", $_REQUEST['signingCerts']);
      if(count($signingCerts) > 1){
        $xadessigner->setSigningChainCount(count($signingCerts)-1);
      }
      for($x = 0; $x < count($signingCerts); $x++){
        $cert = ""; $pass = "";
        $delimitIdx = strpos($signingCerts[$x], ";");
        if($delimitIdx > 0){
          $cert = substr($signingCerts[$x], 0, $delimitIdx);
          $pass = substr($signingCerts[$x], $delimitIdx+1);
        } else {
          $cert = $signingCerts[$x];
        }

        try {
          $certmgr->doImportFromFile($cert, $pass);
        } catch (exception $e) {
          echo "<h2>Failed to load certificate</h2><p>" . $e->getMessage() . "</p>";
          return;
        }

        if($x == 0){
          $xadessigner->setSigningCertHandle($certmgr->getCertHandle());
        } else {
          $xadessigner->setSigningChainHandle($x-1, $certmgr->getCertHandle());
        }
      }

      $xadessigner->doSign();
      echo "<h2>Signing Successful</h2>";
    }
    catch (exception $e) {
      echo "<h2>Signing Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>
