<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - Archive Writer</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - Archive Writer">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>Archive Writer</h2>
<p>A simple Archive Writer sample created with the ArchiveWriter component. Use it to create and modify archives.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/secureblackbox_archivewriter.php');
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
    <h2>Archive Writer Demo</h2>
    
    <b>General Options</b><br/><br/>
    <table>
      <tr><td>Archive File:</td><td><input type=text name=archiveFile value=""></td></tr>
      <tr><td>Archive password:</td><td><input type=password name=archivePass value=""></td></tr>
    </table>

    <br/><br/><b>Archive Options</b><br/><br/>
    <table>
      <tr>
        <td>Archive type:</td>
        <td>
          <select name="arcType">
            <option value="ARCHIVEWRITER_ARCHIVETYPE_ZIP">Zip</option>
            <option value="ARCHIVEWRITER_ARCHIVETYPE_GZIP">GZip</option>
            <option value="ARCHIVEWRITER_ARCHIVETYPE_BZIP_2">BZip2</option>
            <option value="ARCHIVEWRITER_ARCHIVETYPE_TAR">Tar</option>
            <option value="ARCHIVEWRITER_ARCHIVETYPE_TAR_GZIP">Tar_GZip</option>
            <option value="ARCHIVEWRITER_ARCHIVETYPE_TAR_BZIP_2">Tar_BZip2</option>
          </select>
        </td>
      </tr>
      <tr>
        <td>Compression level:</td>
        <td>
          <select name="compressLevel">
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
    </table>
    <br/>
    <br/>

    <b>Files</b>
    <p>Enter the path(s) for any file(s), one per line.</p>
    <br/>
    <textarea style="font-family: Arial, sans-serif; width: 100%" name=filePaths rows=10></textarea>
    <br/>
    <br/>

    <input type="submit" value="Pack" />
  </form>
</div><br/>

<?php

function translateArchiveType($arcType){
  switch($arcType){
    case "ARCHIVEWRITER_ARCHIVETYPE_ZIP":  return ARCHIVEWRITER_ARCHIVETYPE_ZIP;  break;
    case "ARCHIVEWRITER_ARCHIVETYPE_GZIP":  return ARCHIVEWRITER_ARCHIVETYPE_GZIP;  break;
    case "ARCHIVEWRITER_ARCHIVETYPE_BZIP_2":  return ARCHIVEWRITER_ARCHIVETYPE_BZIP_2;  break;
    case "ARCHIVEWRITER_ARCHIVETYPE_TAR":  return ARCHIVEWRITER_ARCHIVETYPE_TAR;  break;
    case "ARCHIVEWRITER_ARCHIVETYPE_TAR_GZIP":  return ARCHIVEWRITER_ARCHIVETYPE_TAR_GZIP;  break;
    case "ARCHIVEWRITER_ARCHIVETYPE_TAR_BZIP_2":  return ARCHIVEWRITER_ARCHIVETYPE_TAR_BZIP_2;  break;
    default:  return ARCHIVEWRITER_ARCHIVETYPE_UNKNOWN;  break;
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $archivewriter = new SecureBlackbox_ArchiveWriter();

    try {
      // General options
      $archiveFile = $_REQUEST['archiveFile'];
      $archivePass = $_REQUEST['archivePass'];

      $archivewriter->setCompressionLevel($_REQUEST['compressLevel']);

      if (strlen($archivePass) > 0)
      {
        $archivewriter->setEncryptionType(ARCHIVEWRITER_ENCRYPTIONTYPE_GENERIC);
        $archivewriter->setEncryptionPassword($archivePass);
      }

      $archivewriter->doCreateNew(translateArchiveType($_REQUEST['arcType']));

      // Add files
      $filePaths = trim($_REQUEST['filePaths']);
      if (strlen($filePaths) > 0) {
        $files = explode("\r\n", $filePaths);
        for($x = 0; $x < count($files); $x++){
          $file = $files[$x];
          if (is_file($file)) {
            $archivewriter->doAddFile(basename($file ), $file );
          }
          elseif (is_dir($file)) {
            $archivewriter->doAddFiles("", $file, TRUE);
          }
        }
      }

      // Save
      $archivewriter->doSave($archiveFile);
      echo "<h2>Archive successfully create</h2>";
    }
    catch (exception $e) {
      echo "<h2>Packing Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
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
