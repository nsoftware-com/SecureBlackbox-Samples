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