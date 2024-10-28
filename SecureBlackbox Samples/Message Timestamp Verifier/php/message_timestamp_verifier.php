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
require_once('../include/secureblackbox_messageverifier.php');
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