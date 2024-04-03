<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - Simple PDF Signer</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - Simple PDF Signer">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>Simple PDF Signer</h2>
<p>An easy-to-use PDF signing example. Supported PKCS11 and Win32 storages.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/secureblackbox_simplepdfsigner.php');
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
<br/>
<br/>
<br/>
<hr/>
NOTE: These pages are simple demos, and by no means complete applications.  They
are intended to illustrate the usage of the SecureBlackbox objects in a simple,
straightforward way.  What we are hoping to demonstrate is how simple it is to
program with our components.  If you want to know more about them, or if you have
questions, please visit <a href="http://www.nsoftware.com/?demopg-SBPHA" target="_blank">www.nsoftware.com</a> or
contact our technical <a href="http://www.nsoftware.com/support/">support</a>.
<br/>
<br/>
Copyright (c) 2024 /n software inc.
<br/>
<br/>
</div>

<div id="footer">
<center>
SecureBlackbox 2022 - Copyright (c) 2024 /n software inc. - For more information, please visit our website at <a href="http://www.nsoftware.com/?demopg-SBPHA" target="_blank">www.nsoftware.com</a>.
</center>
</div>

</body>
</html>

<?php if ($sendBuffer) ob_end_flush(); else ob_end_clean(); ?>
