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
require_once('../include/secureblackbox_pkcs11certificatestorage.php');
require_once('../include/secureblackbox_pdfsigner.php');
require_once('../include/secureblackbox_pdfverifier.php');
require_once('../include/secureblackbox_certificatestorage.php');
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

      $slots = preg_split("/\r\n|\n|\r/", $certstor->doListStores());

      for ($i = 0; $i < count($slots); $i++)
      {
        $desc = $slots[$i];
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