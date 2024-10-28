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
require_once('../include/secureblackbox_pgpwriter.php');
require_once('../include/secureblackbox_pgpkeyring.php');
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
    <h2>PGP Writer Demo</h2>
    
    <h3>General Options</h3>
    <table>
      <tr><td>Input File:</td><td><input type=text name=inputFile value=""></td></tr>
      <tr><td>Output File:</td><td><input type=text name=outputFile value=""></td></tr>
    </table>
    <br/>
    <br/>

    <b>Keys</b><br/>
    <table>
      <tr><td>Public keyring:</td><td><input type=text name=publicKey value=""></td></tr>
      <tr><td>Secret keyring:</td><td><input type=text name=secretKey value=""></td></tr>
      <tr><td>Key Passphrase:</td><td><input type=password name=sPass value=""></td></tr>
    </table>
    <br/>
    <br/>

    <input type="submit" value="Encrypt and Sign" />
  </form>
</div><br/>

<?php
class MyPGPWriter extends SecureBlackbox_PGPWriter
{
  private $password;
  
  function setPassword($value){
    $this->password = $value;

    return 0;
  }
  
  function fireKeyPassphraseNeeded($param){
    echo $this->password;
    $param['passphrase'] = $this->password;

    return $param;
  }
}

  if ($_SERVER['REQUEST_METHOD'] == "POST") {
    $pgpwriter = new MyPGPWriter();
    $pgpkeyring = new SecureBlackbox_PGPKeyring();
    
    try {
      // General options
      $inputFile = $_REQUEST['inputFile'];
      $outputFile = $_REQUEST['outputFile'];
      
      // Keys
      $pgpkeyring->doImportFromFile($_REQUEST['publicKey']);
      $pgpkeyring->doImportFromFile($_REQUEST['secretKey']);

      for ($i = 0; $i < $pgpkeyring->getKeyCount(); $i++) {
        if ($pgpkeyring->getKeyIsSecret($i))
        {
          $pgpwriter->setSigningKeyCount(1);
          $pgpwriter->setSigningKeyHandle(0, $pgpkeyring->getKeyHandle($i));
          break;
        }
      }

      if ($pgpwriter->getSigningKeyCount() == 0)
      {
        echo "<h2>Secret keys not found</h2>";
        return 2;
      }

      for ($i = 0; $i < $pgpkeyring->getKeyCount(); $i++) {
        if ($pgpkeyring->getKeyIsPublic($i))
        {
          $pgpwriter->setEncryptingKeyCount(1);
          $pgpwriter->setEncryptingKeyHandle(0, $pgpkeyring->getKeyHandle($i));
          break;
        }
      }

      if ($pgpwriter->getEncryptingKeyCount() == 0)
      {
        echo "<h2>Public keys not found</h2>";
        return 2;
      }

      $pgpwriter->setPassword($_REQUEST['sPass']);

      $pgpwriter->setInputfile($inputFile);
      $pgpwriter->setOutputfile($outputFile);
      $pgpwriter->doEncryptAndSign();
      echo "<h2>The files were encrypted and signed successfully</h2>";
    }
    catch (exception $e) {
      echo "<h2>Signing Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }
  }
?>