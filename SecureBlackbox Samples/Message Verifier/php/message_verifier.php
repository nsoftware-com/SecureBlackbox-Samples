<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - Message Verifier</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - Message Verifier">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>Message Verifier</h2>
<p>This sample illustrates the verification of signed PKCS7 documents. For advanced validations that include certificate chain processing see CAdESVerifier.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/secureblackbox_messageverifier.php');
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
    <h2>Message Verifying Demo</h2>
    
    <b>General Options</b><br/><br/>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Output/data file:</td><td><input type=text name=outputFile value=""></td></tr>
    </table>
    <label> <input type="checkbox" name="detached" value="1" size="25"/> Detached </label>

    <br/><br/><br/>

    <b>Additional Certificates</b>
    <p>Enter the path(s) for any certificate(s), one per line. If a password is required add a semicolon followed by the password (e.g. C:\path\to\my.pfx;password).</p>
    <table>
      <tr><td>Known Certificates:</td><td><textarea style="font-family: Arial, sans-serif; width: 100%" name=knownCerts rows=10></textarea></td></tr>
    </table>

    <input type="submit" value="Verify" />
  </form>
</div><br/>

<?php
  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $messageverifier = new SecureBlackbox_MessageVerifier();
    $certmgr = new SecureBlackBox_CertificateManager();

    try {
      // General options
      $messageverifier->setInputFile($_REQUEST['inputFile']);

      $detached = (isset($_REQUEST['detached']) && (($_REQUEST['detached'] == 'yes') || ($_REQUEST['detached'] == '1')));

      if ($detached)
      {
        $messageverifier->setDataFile($_REQUEST['outputFile']);
      }
      else
      {
        $messageverifier->setOutputFile($_REQUEST['outputFile']);
      }

      // Known certificates
      $certPaths = trim($_REQUEST['knownCerts']);
      if (strlen($certPaths) > 0) {
        $knownCerts = explode("\r\n", $certPaths);
        $messageverifier->setKnownCertCount(count($knownCerts));
        for($x = 0; $x < count($knownCerts); $x++){
          $cert = ""; $pass = "";
          $delimitIdx = strpos($knownCerts[$x], ";");
          if($delimitIdx > 0){
            $cert = substr($knownCerts[$x], 0, $delimitIdx);
            $pass = substr($knownCerts[$x], $delimitIdx+1);
          } else {
            $cert = $knownCerts[$x];
          }
          $certmgr->doImportFromFile($cert, $pass);
          $messageverifier->setKnownCertHandle($x, $certmgr->getCertHandle());
        }
      }

      // Verification
      if ($detached)
      {
        $messageverifier->doVerifyDetached();
      }
      else
      {
        $messageverifier->doVerify();
      }

      switch($messageverifier->getSignatureValidationResult())
      {
        case 0:  echo "<h2>Signature validated successfully</h2>";          break;
        case 2:  echo "<h2>Signature is invalid</h2>";      break;
        case 3:  echo "<h2>Signer not found</h2>"; break;
        case 4:  echo "<h2>Signature verification failed</h2>";        break;
        default: echo "<h2>Unknown</h2>";        break;
      }
    }
    catch (exception $e) {
      echo "<h2>Verification Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
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
Copyright (c) 2023 /n software inc.
<br/>
<br/>
</div>

<div id="footer">
<center>
SecureBlackbox 2022 - Copyright (c) 2023 /n software inc. - For more information, please visit our website at <a href="http://www.nsoftware.com/?demopg-SBPHA" target="_blank">www.nsoftware.com</a>.
</center>
</div>

</body>
</html>

<?php if ($sendBuffer) ob_end_flush(); else ob_end_clean(); ?>
