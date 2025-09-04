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

public class passwordvault extends ConsoleDemo {
	private static void displayHelp(String errMes) {
		System.out.println(
				"NAME\n" +
						"  passwordvault -- SecureBlackbox PasswordVault Demo Application\n\n" +
						"SYNOPSIS\n" +
						"  passwordvault <-set|-get|-list|-del> [-entry <entry> [-pass <password>]] [-field <field> [-value <value>]] <vault-file> [-vaultpass <vault-password>]\n\n" +
						"DESCRIPTION\n" +
						"  PasswordVault demonstrates the usage of PasswordVault from SecureBlackbox.\n" +
						"  Used to create vaults with user information.\n\n" +
						"  The options are as follows:\n\n" +
						"  -set          Whether to add new entry or add/modify field value.\n\n" +
						"  -get          Whether to get field value.\n\n" +
						"  -list         Whether to get list of entries or fields.\n\n" +
						"  -del          Whether to remove entry or field value.\n\n" +
						"  -entry        The entry name.\n\n" +
						"  -pass         The password for the entry.\n\n" +
						"  -field        The field name.\n\n" +
						"  -value        The new value of field.\n\n" +
						"  vault-file    The vault file (Required).\n\n" +
						"  -vaultpass    The password for the vault.\n\n" +
						"EXAMPLES\n" +
						"	passwordvault -list myvault.bin\n\n" +
						"	passwordvault -get -entry myftpserver -pass mypassword -field password myvault.bin\n\n" +
						"	passwordvault -set -entry myftpserver -field username -value newusername myvault.bin\n\n" +
						"	passwordvault -set -entry newentry myvault.bin -vaultpass mypassword\n\n" +
						"	passwordvault -del -entry myftpserver -field username myvault.bin\n"
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
			PasswordVault vault = new PasswordVault();

			boolean list = false;
			boolean get = false;
			boolean set = false;
			boolean del = false;
			String vaultfile = "";
			String vaultpass = "";
			String entry = "";
			String entrypass = "";
			String field = "";
			String value = "";

			int i = 0;
			while (i < args.length) {
				if (args[i].startsWith("-")) {
					if (args[i].equals("-list"))
						list = true;
					else if (args[i].equals("-get"))
						get = true;
					else if (args[i].equals("-set"))
						set = true;
					else if (args[i].equals("-del"))
						del = true;
					else if (args[i].equals("-vaultpass")) {
						vaultpass = args[i + 1]; // args[i+1] corresponds to the value of argument [i]
						i++;
					} else if (args[i].equals("-entry")) {
						entry = args[i + 1];
						i++;
					} else if (args[i].equals("-pass")) {
						entrypass = args[i + 1];
						i++;
					} else if (args[i].equals("-field")) {
						field = args[i + 1];
						i++;
					} else if (args[i].equals("-value")) {
						value = args[i + 1];
						i++;
					}
				} else
					vaultfile = args[i];

				i++;
			}

			if (!(list || get || set || del)) {
				displayHelp("-list or -get or -set or -del is required.");
				return;
			}

			if ((list && (get || set || del)) || (get && (list || set || del)) || (set && (get || list || del)) || (del && (get || set || list))) {
				displayHelp("Use only one -list or -get or -set or -del parameter.");
				return;
			}

			if (vaultfile.isEmpty()) {
				displayHelp("vault-file is required.");
				return;
			}

			vault.setPassword(vaultpass);

			boolean openVault = false;
			boolean saveVault = set || del;
			if (set) {
				File f = new File(vaultfile);
				openVault = f.exists();
			} else
				openVault = true;

			if (openVault) {
				vault.openFile(vaultfile);
			}


			if (list) {
				if (entry.isEmpty()) {
					String entries = vault.listEntries();

					System.out.println("Entries: ");
					for (String entryName : entries.split(vault.config("ListDelimiter"))) {
						System.out.println("    " + entryName);
					}
				} else {
					String fields = vault.listFields(entry, true);

					System.out.println("Fields in \"" + entry + "\": ");
					for (String fieldName : fields.split(vault.config("ListDelimiter"))) {
						System.out.println("    " + fieldName);
					}
				}
			} else if (get) {
				if (entry.isEmpty()) {
					displayHelp("-entry is required.");
					return;
				}

				if (field.isEmpty()) {
					displayHelp("-field is required.");
					return;
				}

				vault.setEntryPassword(entrypass);

				value = vault.getEntryValueStr(entry, field);
				System.out.println("Value: " + value);
			} else if (set) {
				if (entry.isEmpty()) {
					displayHelp("-entry is required.");
					return;
				}

				if (field.isEmpty()) {
					vault.addEntry(entry);
					vault.changeEntryPassword(entry, entrypass);
					System.out.println("Entry \"" + entry + "\" successfully added.");
				} else {
					String entries = vault.listEntries();
					boolean found = false;
					for (String entryName : entries.split(vault.config("ListDelimiter"))) {
						if (entryName.equals(entry)) {
							found = true;
							break;
						}
					}
					if (! found) {
						vault.addEntry(entry);
						vault.changeEntryPassword(entry, entrypass);
					}
					vault.setEntryPassword(entrypass);

					vault.setEntryValueStr(entry, field, value, !entrypass.isEmpty());
					System.out.println("Field \"" + field + "\" successfully added/modify.");
				}
			} else // del
			{
				if (entry.isEmpty()) {
					displayHelp("-entry is required.");
					return;
				}

				if (field.isEmpty()) {
					vault.removeEntry(entry);
					System.out.println("Entry \"" + entry + "\" successfully removed.");
				} else {
					vault.removeField(entry, field);
					System.out.println("Field \"" + field + "\" successfully removed.");
				}
			}

			if (saveVault) {
				vault.saveFile(vaultfile);
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



