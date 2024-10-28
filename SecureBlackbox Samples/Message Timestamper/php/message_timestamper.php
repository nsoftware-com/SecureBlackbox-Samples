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
require_once('../include/secureblackbox_messagetimestamper.php');
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
    <h2>Message Timestamper Demo</h2>
    
    <h3>General Options</h3>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Output File:</td><td><input type=text name=outputFile value=""></td></tr>
    </table>
    <br/>
    <br/>

    <b>Timestamping options</b><br/><br/>
    <label> <input type="checkbox" name="detached" value="1" size="25"/> Detached </label>
    <br/>
    <br/>

    <table>
      <tr><td>Timestamp Server:</td><td><input type=text name=timestampServer value=""></td></tr>
    </table>
    <br/>
    <br/>
    <br/>

    <input type="submit" value="Timestamp" />
  </form>
</div><br/>

<?php
  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $messagetimestamper = new SecureBlackbox_MessageTimestamper();
    
    try {
      // General options
      $messagetimestamper->setInputFile($_REQUEST['inputFile']);
      $messagetimestamper->setOutputFile($_REQUEST['outputFile']);
      
      // Timestamping options
      $detached = (isset($_REQUEST['detached']) && (($_REQUEST['detached'] == 'yes') || ($_REQUEST['detached'] == '1')));
      $messagetimestamper->setDetached($detached);
      $messagetimestamper->setTimestampServer($_REQUEST['timestampServer']);


      $messagetimestamper->doTimestamp();
      echo "<h2>The file successfully timestamped</h2>";
    }
    catch (exception $e) {
      echo "<h2>Timestamping Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>