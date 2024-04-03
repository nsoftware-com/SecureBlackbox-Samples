<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - XML Signer</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - XML Signer">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>XML Signer</h2>
<p>This small example shows how to create basic XML signatures with XMLSigner control. See XAdESSigner for more sophisticated signatures.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/secureblackbox_xmlsigner.php');
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
    <h2>XML Signing Demo</h2>
    
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
  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $xmlsigner = new SecureBlackbox_XMLSigner();
    $certmgr = new SecureBlackBox_CertificateManager();
    
    try {
      $xmlsigner->setOutputFile($_REQUEST['outputFile']);
      
      if(!empty($_REQUEST['detached'])){
        $xadessigner->setDataFile($_REQUEST['inputFile']);
        $xadessigner->setDataType(XMLSIGNER_DATATYPE_BINARY);
        $xadessigner->setDataURI(basename($_REQUEST['inputFile']));
        $xmlsigner->setSignatureType(XMLSIGNER_SIGNATURETYPE_DETACHED);
      } else {
        $xmlsigner->setInputFile($_REQUEST['inputFile']);
        $xmlsigner->setSignatureType(XMLSIGNER_SIGNATURETYPE_ENVELOPED);
      }
      $xmlsigner->setCanonicalizationMethod($_REQUEST['canonMethod']);
      $xmlsigner->setHashAlgorithm($_REQUEST['hashAlgo']);

      $xmlsigner->doConfig("IncludeKey=" . (!empty($_REQUEST['includeKey']) ? "True" : "False" ));
      $xmlsigner->doConfig("KeyName=" . $_REQUEST['keyName']);
    
      $signingCerts = explode("\r\n", $_REQUEST['signingCerts']);
      if(count($signingCerts) > 1){
        $xmlsigner->setSigningChainCount(count($signingCerts)-1);
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
          $xmlsigner->setSigningCertHandle($certmgr->getCertHandle());
        } else {
          $xmlsigner->setSigningChainHandle($x-1, $certmgr->getCertHandle());
        }
      }

      $xmlsigner->doSign();
      echo "<h2>Signing Successful</h2>";
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
