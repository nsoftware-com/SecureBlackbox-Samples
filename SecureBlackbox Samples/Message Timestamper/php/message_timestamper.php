<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - Message Timestamper</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - Message Timestamper">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>Message Timestamper</h2>
<p>This example illustrates the creation of PKCS7 timestamped messages.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
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
