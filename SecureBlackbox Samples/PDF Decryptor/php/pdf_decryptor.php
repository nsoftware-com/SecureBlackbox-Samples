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
require_once('../include/secureblackbox_pdfdecryptor.php');
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
    <h2>PDF Decryptor Demo</h2>
    
    <h3>General Options</h3>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Output File:</td><td><input type=text name=outputFile value=""></td></tr>
    </table>
    <br/>
    <br/>

    <b>Decryption Options</b><br/><br/>

    <table>
      <tr><td>Password:</td><td><input type=password name=sPass value=""></td></tr>
    </table>
    <br/>
    <br/>

    <table>
      <tr><td>Certificate File:</td><td><input type=text name=sCertFile value=""></td></tr>
      <tr><td>Certificate Password:</td><td><input type=password name=sCertPass value=""></td></tr>
    </table>
    <br/>
    <br/>

    <input type="submit" value="Decrypt" />
  </form>
</div><br/>

<?php
  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $pdfdecryptor = new SecureBlackbox_PDFDecryptor();
    $certmgr = new SecureBlackBox_CertificateManager();
    
    try {
      // General options
      $pdfdecryptor->setInputFile($_REQUEST['inputFile']);
      $pdfdecryptor->setOutputFile($_REQUEST['outputFile']);
      
      // Decryption options
      $pdfdecryptor->setPassword($_REQUEST['sPass']);

      $CertFile = $_REQUEST['sCertFile'];
      if (strlen($CertFile) > 0) 
      {
        $certmgr->doImportFromFile($CertFile, $_REQUEST['sCertPass']);
        $pdfdecryptor->setDecryptionCertificateHandle($certmgr->getCertHandle());
      }

      $pdfdecryptor->doDecrypt();
      echo "<h2>PDF file successfully decrypted</h2>";
    }
    catch (exception $e) {
      echo "<h2>Decryption Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>