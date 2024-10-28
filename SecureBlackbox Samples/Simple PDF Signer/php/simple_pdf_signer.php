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
require_once('../include/secureblackbox_certificatestorage.php');
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

    <b>Signing Certificate</b>
      <br/><br/>
      <input type="radio" name="certtype" value="certfile" checked/>Certificate file:&nbsp;<input type=text name=sCertFile value="">&nbsp;&nbsp;Password:&nbsp;<input type=password name=sCertPass value="">
      <br/><br/>
      <input type="radio" name="certtype" value="pkcs11"/>PKCS11 storage file:&nbsp;<input type=text name=sPkcs11File value="">&nbsp;&nbsp;PIN:&nbsp;<input type=password name=sPIN value="">
      <br/><br/>
      <input type="radio" name="certtype" value="win32"/>Win32 storage:&nbsp;<input type=text name=Win32store value="My">

    <br/><br/><br/>
    
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

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $pdfsigner = new SecureBlackbox_PDFSigner();
    $certmgr = new SecureBlackBox_CertificateManager();
    $certstor = new SecureBlackBox_CertificateStorage();
    
    try {
      // General options
      $pdfsigner->setInputFile($_REQUEST['inputFile']);
      $pdfsigner->setOutputFile($_REQUEST['outputFile']);
      
      // Signing options
      $certtype = $_REQUEST['certtype'];

      if ($certtype == 'certfile')
      {
        $certmgr->doImportFromFile($_REQUEST['sCertFile'], $_REQUEST['sCertPass']);
        $pdfsigner->setSigningCertHandle($certmgr->getCertHandle());
      }
      else
      {
        if ($certtype =='pkcs11')
        {
          $certstor->doOpen('pkcs11://user:' . $_REQUEST['sPIN'] . '@/' . $_REQUEST['sPkcs11File']);
        }
        else
        {
          $certstor->doOpen('system://?store=' . $_REQUEST['Win32store']);
        }

        $pdfsigner->setSigningCertHandle($certstor->getCertHandle(0));
      }

      
      
      $pdfsigner->setNewSigLevel(PDFSIGNER_SIGNATURELEVEL_BES);
      $pdfsigner->setWidgetInvisible(False);
      $pdfsigner->setIgnoreChainValidationErrors(True);

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