/*
 * SecureBlackbox 2024 Java Edition - Sample Project
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

public class usermanager extends ConsoleDemo {
	private static void displayHelp(String errMes) {
		System.out.println(
				"NAME\n" +
				"  usermanager -- SecureBlackbox UserManager Demo Application\n\n" +
				"SYNOPSIS\n" +
				"  usermanager <-set|-check|-list|-del> <users-file> <-filepass file-password> [-user user_name] [-pass password]\n\n" +
				"DESCRIPTION\n" +
				"  UserManager demonstrates the usage of UserManager from SecureBlackbox.\n" +
				"  Used to create file with user information.\n\n" +
				"  The options are as follows:\n\n" +
				"  -set          Whether to add/modify user.\n\n" +
				"  -check        Whether to verify user password.\n\n" +
				"  -list         Whether to get list of users.\n\n" +
				"  -del          Whether to remove user.\n\n" +
				"  users-file    The users file (Required).\n\n" +
				"  -filepass     The password for the file (Required).\n\n" +
				"  -user         The user name\n\n" +
				"  -pass         The user's password.\n\n" +
				"EXAMPLES\n" +
				"	usermanager -list myusers.bin -filepass password\n\n" +
				"	usermanager -check myusers.bin -filepass password -user myuser -pass mypassword \n\n" +
				"	usermanager -set myusers.bin -filepass password -user myuser -pass newpass \n\n" +
				"	usermanager -set myusers.bin -filepass password -user myuser \n\n" +
				"	usermanager -del myusers.bin -filepass password -user myuser \n"
		);

		if (errMes.length() > 0) {
			System.out.println("Error: " + errMes);
			System.out.println();
		}

		confirmExit();
	}

	private static void confirmExit() {
		System.out.println("Press Enter to exit the demo.");
		input();
	}

	public static void main(String[] args) {
		if (args.length == 0) {
			displayHelp("");
			return;
		}
		try {
			UserManager um = new UserManager();

			boolean list = false;
			boolean check = false;
			boolean set = false;
			boolean del = false;
			String filename = "";
			String filepass = "";
			String user = "";
			String pass = "";

			int i = 0;
			while (i < args.length) {
				if (args[i].startsWith("-")) {
					if (args[i].equals("-list"))
						list = true;
					else if (args[i].equals("-check"))
						check = true;
					else if (args[i].equals("-set"))
						set = true;
					else if (args[i].equals("-del"))
						del = true;
					else if (args[i].equals("-filepass")) {
						filepass = args[i + 1]; // args[i+1] corresponds to the value of argument [i]
						i++;
					} else if (args[i].equals("-user")) {
						user = args[i + 1];
						i++;
					} else if (args[i].equals("-pass")) {
						pass = args[i + 1];
						i++;
					}
				} else
					filename = args[i];

				i++;
			}

			if (!(list || check || set || del)) {
				displayHelp("-list or -check or -set or -del is required.");
				return;
			}

			if ((list && (check || set || del)) || (check && (list || set || del)) || (set && (check || list || del)) || (del && (check || set || list))) {
				displayHelp("Use only one -list or -check or -set or -del parameter.");
				return;
			}

			if (filename.isEmpty()) {
				displayHelp("users-file is required.");
				return;
			}

			if (filepass.isEmpty()) {
				displayHelp("filepass is required.");
				return;
			}

			boolean openFile = false;
			boolean saveFile = set || del;
			if (set) {
				File f = new File(filename);
				openFile = f.exists();
			} else
				openFile = true;

			if (openFile) {
				um.importFromFile(filename, filepass, true);
			}

			if (list) {
				System.out.println(um.getUsers().size() + " user(s) have been found:");

				for (int x = 0; x < um.getUsers().size(); x++) {
					System.out.println("   - User " + (x + 1) + ": " + um.getUsers().item(x).getUsername() + " (" +
							(um.getUsers().item(x).getPassword().length() > 0 ? "with password" : "no password") + ")");
				}
			} else if (check) {
				if (user.isEmpty()) {
					displayHelp("-user is required.");
					return;
				}

				if (pass.isEmpty()) {
					displayHelp("-pass is required.");
					return;
				}

				if (um.verifyUser(user, pass))
				{
					System.out.println("Password is correct");
				}
				else
				{
					System.out.println("Password is incorrect");
				}
			} else if (set) {
				if (user.isEmpty()) {
					displayHelp("-user is required.");
					return;
				}

				int Idx = -1;
				for (int x = 0; x < um.getUsers().size(); x++) {
					if (user.equals(um.getUsers().item(x).getUsername())) {
						Idx = x;
						break;
					}
				}

				if (Idx == -1)
				{
					Idx = um.addUser(user);
					um.getUsers().item(Idx).setPassword(pass);
					System.out.println("New user added");
				}
				else
				{
					um.getUsers().item(Idx).setPassword(pass);
					System.out.println("User has been changed");
				}
			} else // del
			{
				if (user.isEmpty()) {
					displayHelp("-user is required.");
					return;
				}

				boolean found = false;
				for (int x = 0; x < um.getUsers().size(); x++) {
					if (user.equals(um.getUsers().item(x).getUsername())) {
						um.getUsers().remove(x);
						found = true;
						break;
					}
				}

				if (found)
				{
					System.out.println("User has been deleted");
				}
				else
				{
					System.out.println("User not found");
				}
			}

			if (saveFile) {
				um.exportToFile(filename, filepass);
			}

			System.out.println();
			confirmExit();
		} catch (Exception ex) {
			displayError(ex);
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
  static String prompt(String label, String punctuation, String defaultVal) {
      System.out.print(label + " [" + defaultVal + "]" + punctuation + " ");
      String response = input();
      if (response.equals(""))
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

  /**
   * Takes a list of switch arguments or name-value arguments and turns it into a map.
   */
  static java.util.Map<String, String> parseArgs(String[] args) {
    java.util.Map<String, String> map = new java.util.HashMap<String, String>();
    
    for (int i = 0; i < args.length; i++) {
      // Add a key to the map for each argument.
      if (args[i].startsWith("-")) {
        // If the next argument does NOT start with a "-" then it is a value.
        if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
          // Save the value and skip the next entry in the list of arguments.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), args[i + 1]);
          i++;
        } else {
          // If the next argument starts with a "-", then we assume the current one is a switch.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), "");
        }
      } else {
        // If the argument does not start with a "-", store the argument based on the index.
        map.put(Integer.toString(i), args[i].toLowerCase());
      }
    }
    return map;
  }
}



