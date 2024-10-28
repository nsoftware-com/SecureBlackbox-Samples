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
require_once('../include/secureblackbox_messagedecompressor.php');
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
    <h2>Message Decompressor Demo</h2>
    
    <h3>General Options</h3>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Output File:</td><td><input type=text name=outputFile value=""></td></tr>
    </table>
    <br/>
    <br/>

    <input type="submit" value="Decompress" />
  </form>
</div><br/>

<?php
  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $messagedecompressor = new SecureBlackbox_MessageDecompressor();
    
    try {
      // General options
      $messagedecompressor->setInputFile($_REQUEST['inputFile']);
      $messagedecompressor->setOutputFile($_REQUEST['outputFile']);

      $messagedecompressor->doDecompress();
      echo "<h2>The file successfully decompressed</h2>";
    }
    catch (exception $e) {
      echo "<h2>Decompressing Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>