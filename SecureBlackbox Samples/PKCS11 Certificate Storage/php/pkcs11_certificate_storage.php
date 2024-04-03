<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - PKCS11 Certificate Storage</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - PKCS11 Certificate Storage">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>PKCS11 Certificate Storage</h2>
<p>An easy-to-use Certificate Storage for work with PKCS11 storages.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/secureblackbox_pkcs11certificatestorage.php');
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
    <h2>PDF Signing Demo</h2>
    
    <h3>General Options</h3>
    <table>
      <tr><td>Storage File:</td><td><input type=text name=storageFile value=""></td></tr>
      <tr><td>PIN:</td><td><input type=password name=sPIN value=""></td></tr>
    </table>
    <br/>
    <br/>

    <input type="submit" value="Open" />
  </form>
</div><br/>

<?php

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $certstor = new SecureBlackBox_CertificateStorage();
    $certstor_dop = new SecureBlackBox_CertificateStorage();
    
    try
    {
      $certstor->doOpen('pkcs11:///' . $_REQUEST['storageFile'] . '?slot=-1');

      $slotCount = $certstor->doConfig('PKCS11SlotCount');

      for ($i = 0; $i < $slotCount; $i++)
      {
        $desc = $certstor->doConfig('PKCS11SlotDescription[' . $i . ']');
        $active = $certstor->doConfig('PKCS11SlotTokenPresent[' . $i . ']');
        
        if ($desc != '')
        {
          if ($active == 'True')
          {
            
            echo $desc . ': <br/>';

            $certstor_dop->doOpen('pkcs11://user:' . $_REQUEST['sPIN'] . '@/' . $_REQUEST['storageFile'] . '?slot=' . $i);
            
            for ($j = 0; $j < $certstor_dop->getCertCount(); $j++)
            {
              echo '&nbsp;&nbsp;Subject: ' . $certstor_dop->getCertSubject($j) . '<br/>';
              echo '&nbsp;&nbsp;Issuer: ' . $certstor_dop->getCertIssuer($j) . '<br/>';
              echo '&nbsp;&nbsp;ValidFrom: ' . $certstor_dop->getCertValidFrom($j) . '<br/>';
              echo '&nbsp;&nbsp;ValidTo: ' . $certstor_dop->getCertValidTo($j) . '<br/>';
              echo '&nbsp;&nbsp;Key: ' . $certstor_dop->getCertKeyAlgorithm($j) . '  (' . $certstor_dop->getCertKeyBits($j) . ') <br/><br/>';
            }
          }
          else
          {
            echo $desc . ': No token <br/><br/>';
          }
        }
      }
    }
    catch (exception $e) {
      echo "<h2>Opening Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
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
