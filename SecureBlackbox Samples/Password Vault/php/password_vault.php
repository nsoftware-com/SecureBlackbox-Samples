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
          $entries = preg_split("/\r\n|\n|\r/", $vault->doListEntries());

          echo "<h2>Entries: </h2>";
          for($x = 0; $x < count($entries); $x++) {
            echo "<h3>" . $entries[$x] . "</h3>";
          }
        }
        else
        {
          $fields = preg_split("/\r\n|\n|\r/", $vault->doListFields($_REQUEST['entryName'], True));

          echo"<h2>Fields in " . $_REQUEST['entryName'] . ": </h2>";
          for($x = 0; $x < count($fields); $x++) {
            echo "<h3>" . $fields[$x] . "</h3>";
          }
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
          try {
            $vault->doAddEntry($_REQUEST['entryName']);
          }
          catch (exception $e) {}
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