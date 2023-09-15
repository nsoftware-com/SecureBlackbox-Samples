/*
 * SecureBlackbox 2022 Java Edition - Sample Project
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

import java.io.*;
import secureblackbox.*;

public class simpleauthenticator extends ConsoleDemo {

//Users:
//      Name      Password        HOTP

//      user      password

//      admin     adminpassword   1862287066

	public static void main(String[] args) {
		if (args.length < 6) {
			System.out.println("usage: simpleauthenticator [-id user_id] [-users users_file] [-filepass password_for_users_file]");
			System.out.println("Options: ");
			System.out.println("  -id          An user id for authentication (Required).");
			System.out.println("  -users       The file with users list (Required).");
			System.out.println("  -filepass    The password for the users file.");
			System.out.println("\r\nExample: simpleauthenticator -id UserId1 -users C:\\auth\\users.usr -filepass mypassword");
		}
		else {
			try {
				Authenticator auth = new Authenticator();
				System.out.println("***************************************************************************************************");
				System.out.println("* This sample shows how to use authenticator. *");
				System.out.println("***************************************************************************************************\n");

				String userid = "";
				String usersFile = "";
				String usersPass = "";

				for (int i = 0; i < args.length; i++) {
					if (args[i].startsWith("-")) {
						if (args[i].equals("-id"))
							userid = args[i + 1];   // args[i+1] corresponds to the value of argument [i]
						if (args[i].equals("-users"))
							usersFile = args[i + 1];
						if (args[i].equals("-filepass"))
							usersPass = args[i + 1];
					}
				}

				if (userid.isEmpty())
				{
					System.out.println("-id is required");
					return;
				}

				if (usersFile.isEmpty())
				{
					System.out.println("-users is required");
					return;
				}

				Usermanager um = new Usermanager();
				um.load(usersFile, usersPass);

				auth.setUsers(um.getUsers());

				boolean userFind = false;
				for (int i = 0; i < auth.getUsers().size(); i++)
				{
					if (auth.getUsers().item(i).getUsername().equals(userid))
					{
						userFind = true;
						break;
					}
				}

				if (!userFind)
				{
					System.out.println("User not found. Authentication failed");
					return;
				}

				BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
				char dm = (char) 34;

				int res = auth.startAuth(userid);

				while (res == 0) // Further Auth Needed
				{
					System.out.println("Enter auth token for " + dm + auth.getAuthInfo().getAuthMethod() + dm + " authentication");

					String authtoken = reader.readLine();

					res = auth.continueAuth(auth.getAuthInfo().getState(), authtoken);
				}

				if (res == 1) // Succeeded
					System.out.println("Authentication succeeded");
				else
				if (res == 2) // Failed
					System.out.println("Authentication failed");
				else
					System.out.println("Canceled by user");
			}
			catch (Exception ex)
			{
				displayError(ex);
			}
		}
	}
}
class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }

  static String prompt(String label, String punctuation, String defaultVal)
  {
	System.out.print(label + " [" + defaultVal + "] " + punctuation + " ");
	String response = input();
	if(response.equals(""))
		return defaultVal;
	else
		return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof SecureBlackboxException) {
      System.out.print(" (" + ((SecureBlackboxException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }
}



