<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - Password Vault</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - Password Vault">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>Password Vault</h2>
<p>A simple Password Vault to save user's information and passwords.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/secureblackbox_passwordvault.php');
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
    <h2>Password vault Demo</h2>
    
    <h3>General Options</h3>
    <label><input type ="radio" checked="checked" name="comtype" value="list" /> List </label>
    <label><input type ="radio" name="comtype" value="get" /> Get </label>
    <label><input type ="radio" name="comtype" value="set" /> Set </label>
    <label><input type ="radio" name="comtype" value="del" /> Delete </label>
    <br/>
    <br/>

    <table>
      <tr><td>Vault File:</td><td><input type=text name=vaultFile value=""></td></tr>
      <tr><td>Password:</td><td><input type=password name=vaultPass value=""></td></tr>
    </table>
    <br/>
    <br/>

    <table>
      <tr><td>Entry name:</td><td><input type=text name=entryName value=""></td></tr>
      <tr><td>Entry password:</td><td><input type=password name=entryPass value=""></td></tr>
      <tr><td>Field name:</td><td><input type=text name=fieldName value=""></td></tr>
      <tr><td>Value:</td><td><input type=text name=fieldValue value=""></td></tr>
    </table>
    <br/>
    <br/>

    <input type="submit" value="Do" />
  </form>
</div><br/>

<?php
  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $vault = new SecureBlackbox_PasswordVault();
    
    try {
      // General options
      $vaultFile = $_REQUEST['vaultFile'];

      if ($vaultFile == "")
      {
        echo "<h2>Error: Please set Vault File</h2>";
        exit;
      }

      $vault->setPassword($_REQUEST['vaultPass']);

      $comtype = $_POST["comtype"];

      $openVault = False;
      $saveVault = ($comtype == "set") || ($comtype == "del");

      if ($comtype == "set")
        $openVault = file_exists($vaultFile);
      else
        $openVault = True;

      if ($openVault)
        $vault->doOpenFile($vaultFile);

      if ($comtype == "list")
      {
        if ($_REQUEST['entryName'] == "")
        {
          $entries = $vault->doListEntries();

          echo "<h2>Entries: </h2><p>" . $entries . "</p>";
        }
        else
        {
          $fields = $vault->doListFields($_REQUEST['entryName'], True);

          echo"<h2>Fields in " . $_REQUEST['entryName'] . ": </h2><p>" . $fields . "</p>";
        }
      }
      else
      if ($comtype == "get")
      {
        if ($_REQUEST['entryName'] == "")
        {
          echo "<h2>Error: Please set Entry name</h2>";
          exit;
        }

        if ($_REQUEST['fieldName'] == "")
        {
          echo "<h2>Error: Please set Field name</h2>";
          exit;
        }

        $vault->setEntryPassword($_REQUEST['entryPass']);

        $value = $vault->doGetEntryValueStr($_REQUEST['entryName'], $_REQUEST['fieldName']);
        echo "<h2>Value: </h2><p>" . $value . "</p>";
      }
      else
      if ($comtype == "set")
      {
        if ($_REQUEST['entryName'] == "")
        {
          echo "<h2>Error: Please set Entry name</h2>";
          exit;
        }

        if ($_REQUEST['fieldName'] == "")
        {
          $vault->doAddEntry($_REQUEST['entryName']);
          echo "<h2>Entry " . $_REQUEST['entryName'] . " successfully added.</h2>";
        }
        else 
        {
          $vault->setEntryPassword($_REQUEST['entryPass']);

          $vault->doSetEntryValueStr($_REQUEST['entryName'], $_REQUEST['fieldName'], $_REQUEST['fieldValue'], ($_REQUEST['entryPass'] != ""));
          echo "<h2>Field " . $_REQUEST['fieldName'] . " successfully added/modify.</h2>";
        }
      }
      else // del
      {
        if ($_REQUEST['entryName'] == "")
        {
          echo "<h2>Error: Please set Entry name</h2>";
          exit;
        }

        if ($_REQUEST['fieldName'] == "")
        {
          $vault->doRemoveEntry($_REQUEST['entryName']);
          echo "<h2>Entry " . $_REQUEST['entryName'] . " successfully removed.</h2>";
        }
        else 
        {
          $vault->doRemoveField($_REQUEST['entryName'], $_REQUEST['fieldName']);
          echo "<h2>Field " . $_REQUEST['fieldName'] . " successfully removed.</h2>";
        }
      }

      if ($saveVault)
        $vault->doSaveFile($vaultFile);
    }
    catch (exception $e) {
      echo "<h2>Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
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
