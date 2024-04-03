<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>SecureBlackbox 2022 Demos - Simple Authenticator</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="SecureBlackbox 2022 Demos - Simple Authenticator">
</head>

<body>

<div id="content">
<h1>SecureBlackbox - Demo Pages</h1>
<h2>Simple Authenticator</h2>
<p>A simple Authenticator created with the Authenticator component. Use it to user authentication.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
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
      $usrmgr->doLoad("users.usr", "password");

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
