<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - Authenticode Signer</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - Authenticode Signer">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>Authenticode Signer</h2>
<p>A simple authenticode signer created with the AuthenticodeSigner component. Use it to sign EXE and DLL files in accordance with MS Authenticode technology.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/secureblackbox_authenticodesigner.php');
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
    <h2>Authenticode Signing Demo</h2>
    
    <h3>General Options</h3>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Output File:</td><td><input type=text name=outputFile value=""></td></tr>
    </table>
    <br/>
    <br/>

    <b>Signing Certificate</b>
    <table>
      <tr><td>Certificate File:</td><td><input type=text name=sCertFile value=""></td></tr>
      <tr><td>Password:</td><td><input type=password name=sCertPass value=""></td></tr>
    </table>
    <br/>
    <br/>

    <b>Signature Settings</b>
    <table>
      <tr><td>Description:</td><td><input type=text name=description value=""></td></tr>
      <tr><td>URL:</td><td><input type=text name=url value=""></td></tr>
      <tr><td>Hash Algorithm:</td><td>
        <select name="hashAlg">
          <option value=""></option>
          <option value="SHA1">SHA1</option>
          <option value="MD5">MD5</option>
          <option value="SHA256">SHA256</option>
          <option value="SHA384">SHA384</option>
          <option value="SHA512">SHA512</option>
          <option value="RIPEMD160">RIPEMD160</option>
        </select>
      </td></tr>
    </table>
    <br/>

    <b>Statement</b><br/>
    <div style="display: inline-block; margin: .25em 0;">
      <input type=radio id=statement1 name=statement value="Individual" checked />
      <label for=statement1>Individual</label>
      &nbsp;&nbsp;&nbsp;
      <input type=radio id=statement2 name=statement value="Commercial" />
      <label for=statement2>Commercial</label>
    </div><br/><br/>

    <b>Timestamp</b><br/><br/>
    <input type=checkbox id="useTimestamp" name="useTimestamp" /><label for=useTimestamp>Request a timestamp from TSA server</label>
    <table>
      <tr><td>Timestamp Server:</td><td><input type=text name=timestampServer value=""></td></tr>
    </table>
    <input type="submit" value="Sign" />
  </form>
</div><br/>

<?php
  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $authenticodesigner = new SecureBlackbox_AuthenticodeSigner();
    $certmgr = new SecureBlackBox_CertificateManager();
    
    try {
      // General options
      $authenticodesigner->setInputFile($_REQUEST['inputFile']);
      $authenticodesigner->setOutputFile($_REQUEST['outputFile']);
      
      // Signing options
      $certmgr->doImportFromFile($_REQUEST['sCertFile'], $_REQUEST['sCertPass']);
      $authenticodesigner->setSigningCertHandle($certmgr->getCertHandle());
      
      // Additional options
      $authenticodesigner->setSignatureDescription($_REQUEST['description']);
      $authenticodesigner->setSignatureURL($_REQUEST['url']);
      $hashAlg = $_POST['hashAlg'];
      if (!empty($hashAlg)) {$authenticodesigner->setHashAlgorithm($hashAlg);}

      $statement = $_POST["statement"];
      if ($statement == "Individual")
      {
        $authenticodesigner->setStatementType(AUTHENTICODESIGNER_STATEMENTTYPE_INDIVIDUAL);
      } 
      else
      {
        $authenticodesigner->setStatementType(AUTHENTICODESIGNER_STATEMENTTYPE_COMMERCIAL);
      }

      $authenticodesigner->setTimestampServer($_REQUEST['timestampServer']);
    
      $authenticodesigner->doSign();
      echo "<h2>Signing Successful</h2>";
    }
    catch (exception $e) {
      echo "<h2>Signing Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
      echo "<p>" . $authenticodesigner->getStatementType() . "</p>";
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
