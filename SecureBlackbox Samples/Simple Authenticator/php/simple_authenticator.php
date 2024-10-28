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
require_once('../include/secureblackbox_authenticator.php');
require_once('../include/secureblackbox_usermanager.php');
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
  <?php 
    $auth = new SecureBlackbox_Authenticator();
    $usrmgr = new SecureBlackBox_Usermanager();

    try {
      $usrmgr->doImportFromFile("users.usr", "password", true);

      $auth->setUserCount($usrmgr->getUserCount());
      for($x = 0; $x < $usrmgr->getUserCount(); $x++){
        $auth->setUserHandle($x, $usrmgr->getUserHandle($x));
      }
    }
    catch (exception $e) {
      echo "<h2>Loading users Failure (Details Below)</h2><p>" . $e->getMessage() . "</p>";
    }

    function continueAuthentication($res, $authmethod, $state){
      if ($res == 0) {
        echo "<h2>Simple authenticator demo</h2>";
        echo "<table>";
        echo "  <tr><td>Auth method:</td><td><input type=text name=authmethod value=\"" . $authmethod . "\" readonly=\"readonly\"></td></tr>";
        echo "  <tr><td>Auth token:</td><td><input type=text name=authtoken value=\"\"></td></tr>";
        echo "</table>";
        echo "<input type=text name=state value=\"" . $state . "\" hidden>";
        echo "<br/>";
        echo "<br/>";
        echo "<input type=\"submit\" name=\"Continue\" value=\"Continue\" />";
      }
      elseif ($res == 1) {
        echo "Authentication succeeded";
      } 
      elseif ($res == 2) {
        echo "Authentication failed";
      }
    }

    if(isset($_POST['Start'])) {
      $userid = $_REQUEST['userid'];

      $res = $auth->doStartAuth($userid);

      continueAuthentication($res, $auth->getAuthInfoAuthMethod(), $auth->getAuthInfoState());
    } 
    elseif(isset($_POST['Continue'])) {
        $authtoken = $_REQUEST['authtoken'];
        $state = $_REQUEST['state'];

        $res = $auth->doContinueAuth($state, $authtoken);

        continueAuthentication($res, $auth->getAuthInfoAuthMethod(), $auth->getAuthInfoState());
    }
    else {
      echo "<h2>Simple authenticator demo</h2>";
      echo "<table>";
      echo "  <tr><td>User Id:</td><td><input type=text name=userid value=\"\"></td></tr>";
      echo "</table>";
      echo "<br/>";
      echo "<br/>";
      echo "<input type=\"submit\" name=\"Start\" value=\"Start\" />";
    }
   ?>
   </form>
</div><br/>
