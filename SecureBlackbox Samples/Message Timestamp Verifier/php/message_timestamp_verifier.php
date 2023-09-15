<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - Message Timestamp Verifier</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - Message Timestamp Verifier">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>Message Timestamp Verifier</h2>
<p>This small demo shows how to validate PKCS7 timestamped messages with the MessageTimestampVerifier class.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/secureblackbox_messagetimestampverifier.php');
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
    <h2>Message Timestamp Verifier Demo</h2>
    
    <b>General Options</b><br/><br/>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Output/data file:</td><td><input type=text name=outputFile value=""></td></tr>
    </table>
    <br/>
    <label> <input type="checkbox" name="detached" value="1" size="25"/> Detached </label>
    <br/><br/>

    <input type="submit" value="Verify" />
  </form>
</div><br/>

<?php
  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $messagetimestampverifier = new SecureBlackbox_MessageTimestampVerifier();

    try {
      // General options
      $messagetimestampverifier->setInputFile($_REQUEST['inputFile']);

      $detached = (isset($_REQUEST['detached']) && (($_REQUEST['detached'] == 'yes') || ($_REQUEST['detached'] == '1')));

      if ($detached)
      {
        $messagetimestampverifier->setDataFile($_REQUEST['outputFile']);
      }
      else
      {
        $messagetimestampverifier->setOutputFile($_REQUEST['outputFile']);
      }

      // Verification
      if ($detached)
      {
        $messagetimestampverifier->doVerifyDetached();
      }
      else
      {
        $messagetimestampverifier->doVerify();
      }

      switch($messagetimestampverifier->getSignatureValidationResult())
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
