<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - PDF Encryptor</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - PDF Encryptor">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>PDF Encryptor</h2>
<p>A tiny PDF encryption example which supports password- and certificate-based encryption.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/secureblackbox_pdfencryptor.php');
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
    <h2>PDF Encryptor Demo</h2>
    
    <h3>General Options</h3>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Output File:</td><td><input type=text name=outputFile value=""></td></tr>
    </table>
    <br/>
    <br/>

    <b>Encryption Options</b><br/><br/>
    <table>
      <tr>
        <td>Encryption algorithm:</td>
        <td>
          <select name="encAlg">
            <option value="RC440">RC4/40 bits (Acrobat 4)</option>
            <option value="RC4128">RC4/128 bits (Acrobat 5)</option>
            <option value="AES128">AES/128 bits (Acrobat 6, 7)</option>
            <option value="AES2569">AES/256 bits (Acrobat 9)</option>
            <option value="AES256X">AES/256 bits (Acrobat X)</option>
          </select>
        </td>
      </tr>
    </table>
    <br/>
    <label> <input type="checkbox" name="encryptMetadata" value="1" size="25"/> Encrypt document metadata </label>
    <br/>
    <br/>
    <br/>

    <b><label><input type ="radio" checked="checked" name="enctype" value="Password" /> Password encryption</label><b>
    <br/>
    <br/>
    <table>
      <tr><td>Password:</td><td><input type=password name=sPass value=""></td></tr>
    </table>
    <br/>
    <br/>

    <b><label><input type ="radio" name="enctype" value="Key" /> Public key encryption</label><b>
    <br/>
    <br/>
    <table>
      <tr><td>Certificate File:</td><td><input type=text name=sCertFile value=""></td></tr>
      <tr><td>Password:</td><td><input type=password name=sCertPass value=""></td></tr>
    </table>
    <br/>
    <br/>

    <input type="submit" value="Encrypt" />
  </form>
</div><br/>

<?php
  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $pdfencryptor = new SecureBlackbox_PDFEncryptor();
    $certmgr = new SecureBlackBox_CertificateManager();
    
    try {
      // General options
      $pdfencryptor->setInputFile($_REQUEST['inputFile']);
      $pdfencryptor->setOutputFile($_REQUEST['outputFile']);
      
      // Encryption options
      switch($_REQUEST['encAlg']){
        case "RC4128":
          $pdfencryptor->setEncryptionAlgorithm("RC4");
          $pdfencryptor->doConfig("RC4KeyBits=128");
          break;
        case "AES128":
          $pdfencryptor->setEncryptionAlgorithm("AES128");
          break;
        case "AES2569":
          $pdfencryptor->setEncryptionAlgorithm("AES256");
          $pdfencryptor->doConfig("HardenedKeyGeneration=false");
          break;
        case "AES256X":
          $pdfencryptor->setEncryptionAlgorithm("AES256");
          $pdfencryptor->doConfig("HardenedKeyGeneration=true");
          break;
        default:
          $pdfencryptor->setEncryptionAlgorithm("RC4");
          $pdfencryptor->doConfig("RC4KeyBits=40");
          break;
      }
      
      $encryptMetadata = (isset($_REQUEST['encryptMetadata']) && (($_REQUEST['encryptMetadata'] == 'yes') || ($_REQUEST['encryptMetadata'] == '1')));
      $pdfencryptor->setEncryptMetadata($encryptMetadata);
      
      $enctype = $_POST["enctype"];
      if ($enctype == "Password")
      {
        $pdfencryptor->setEncryptionType(PDFENCRYPTOR_ENCRYPTIONTYPE_PASSWORD);
        $pdfencryptor->setUserPassword($_REQUEST['sPass']);
        $pdfencryptor->setOwnerPassword($_REQUEST['sPass']);
      }
      else
      {
        $pdfencryptor->setEncryptionType(PDFENCRYPTOR_ENCRYPTIONTYPE_CERTIFICATE);
        $certmgr->doImportFromFile($_REQUEST['sCertFile'], $_REQUEST['sCertPass']);
        $pdfencryptor->setEncryptionCertificateHandle($certmgr->getCertHandle());
      }

      $pdfencryptor->doEncrypt();
      echo "<h2>PDF file successfully encrypted</h2>";
    }
    catch (exception $e) {
      echo "<h2>Encryption Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
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
