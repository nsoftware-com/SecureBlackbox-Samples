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
require_once('../include/secureblackbox_messagecompressor.php');
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
    <h2>Message Compressor Demo</h2>
    
    <h3>General Options</h3>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Output File:</td><td><input type=text name=outputFile value=""></td></tr>
    </table>
    <br/>
    <br/>

    <b>Encrypting options</b><br/>
    <table>
      <tr>
        <td>Compression Level:</td>
        <td>
          <select name="compLevel">
            <option value="1">1</option>
            <option value="2">2</option>
            <option value="3">3</option>
            <option value="4">4</option>
            <option value="5">5</option>
            <option selected value="6">6</option>
            <option value="7">7</option>
            <option value="8">8</option>
            <option value="9">9</option>
          </select>
        </td>
      </tr>
      <tr>
        <td>Content type:</td>
        <td><input type=text name=contentType value=""></td>
      </tr>
    </table>
    <br/>
    <br/>

    <input type="submit" value="Compress" />
  </form>
</div><br/>

<?php
  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $messagecompressor = new SecureBlackbox_MessageCompressor();
    
    try {
      // General options
      $messagecompressor->setInputFile($_REQUEST['inputFile']);
      $messagecompressor->setOutputFile($_REQUEST['outputFile']);
      
      // Encrypting options
      $messagecompressor->setCompressionLevel($_REQUEST['compLevel']);
      $messagecompressor->doConfig("ContentType=" . $_REQUEST['contentType']);
      
      $messagecompressor->doCompress();
      echo "<h2>The file successfully compressed</h2>";
    }
    catch (exception $e) {
      echo "<h2>Compressing Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>