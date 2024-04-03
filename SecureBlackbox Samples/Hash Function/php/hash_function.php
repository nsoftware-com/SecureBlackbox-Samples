<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - Hash Function</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - Hash Function">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>Hash Function</h2>
<p>Use this example to learn about calculate hash with HashFunction control.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/secureblackbox_hashfunction.php');
require_once('../include/secureblackbox_cryptokeymanager.php');
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
    <h2>Hash function Demo</h2>
    
    <b>General Options</b><br/><br/>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Password:</td><td><input type=password name=sPass value=""></td></tr>
      <tr>
        <td>Encoding:</td>
        <td>
          <select name="encoding">
            <option value="Binary">Binary</option>
            <option value="Base64">Base64</option>
            <option value="Compact">Compact</option>
            <option value="JSON">JSON</option>
          </select>
        </td>
      </tr>
    </table>

    <br/><br/><br/>
    <input type="submit" value="Hash" />
  </form>
</div><br/>

<?php
  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $hashfunction = new SecureBlackbox_HashFunction();
    $keymgr = new SecureBlackBox_CryptoKeyManager();

    try 
    {
      // Key from password
      $pass = trim($_REQUEST['sPass']);
      if (strlen($pass) > 0)
      {
        $keymgr->doDeriveKey(256, $pass, '');
        
        $hashfunction->setKeyHandle($keymgr->getKeyHandle());
      }

      switch(trim($_REQUEST['encoding'])){
        case "Binary":  $hashfunction->setOutputEncoding(HASHFUNCTION_OUTPUTENCODING_BINARY); break;
        case "Base64":  $hashfunction->setOutputEncoding(HASHFUNCTION_OUTPUTENCODING_BASE_64); break;
        case "Compact":  $hashfunction->setOutputEncoding(HASHFUNCTION_OUTPUTENCODING_COMPACT); break;
        case "JSON":  $hashfunction->setOutputEncoding(HASHFUNCTION_OUTPUTENCODING_JSON); break;
        default: $hashfunction->setOutputEncoding(HASHFUNCTION_OUTPUTENCODING_DEFAULT); break;
      }

      // Hash
      $res = $hashfunction->doHashFile($_REQUEST['inputFile']);

      echo "<h2>Hash value: " . $res . "</h2>";
    }
    catch (exception $e) {
      echo "<h2>Hashing Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
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
