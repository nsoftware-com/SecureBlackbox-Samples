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

public class passwordvault extends ConsoleDemo {

	public static void main(String[] args) {
		if (args.length < 2) {
			System.out.println("usage: passwordvault  (-set|-get|-list|-del) [-entry <entry> [-pass <password>]] [-field <field> [-value <value>]] <vault-file> [-vaultpass <vault-password>]");
			System.out.println("Options: ");
			System.out.println("  -set          Whether to add new entry or add/modify field value.");
			System.out.println("  -get          Whether to get field value.");
			System.out.println("  -list         Whether to get list of entries or fields.");
			System.out.println("  -del          Whether to remove entry or field value.");
			System.out.println("  -vaultpass    The password for the vault");
			System.out.println("  -entry        The entry name");
			System.out.println("  -pass         The password for the entry.");
			System.out.println("  -field        The field name");
			System.out.println("  -value        The new value of field");
			System.out.println("\r\nExample: passwordvault -list myvault.bin");
			System.out.println("             passwordvault -get -entry myftpserver -pass mypassword -field password myvault.bin");
			System.out.println("             passwordvault -set -entry myftpserver -field username -value newusername myvault.bin");
			System.out.println("             passwordvault -set -entry newentry myvault.bin -vaultpass mypassword");
			System.out.println("             passwordvault -del -entry myftpserver -field username myvault.bin");
		} else {
			try {
				Passwordvault vault = new Passwordvault();
				System.out.println("***************************************************************************************************");
				System.out.println("* This sample illustrates how to use password vault. *");
				System.out.println("***************************************************************************************************\n");

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
						else
						if (args[i].equals("-get"))
							get = true;
						else
						if (args[i].equals("-set"))
							set = true;
						else
						if (args[i].equals("-del"))
							del = true;
						else
						if (args[i].equals("-vaultpass")) {
							vaultpass = args[i + 1]; // args[i+1] corresponds to the value of argument [i]
							i++;
						}
						else
						if (args[i].equals("-entry")) {
							entry = args[i + 1];
							i++;
						}
						else
						if (args[i].equals("-pass")) {
							entrypass = args[i + 1];
							i++;
						}
						else
						if (args[i].equals("-field")) {
							field = args[i + 1];
							i++;
						}
						else
						if (args[i].equals("-value")) {
							value = args[i + 1];
							i++;
						}
					}
					else
						vaultfile = args[i];

					i++;
				}

				if (!(list || get || set || del))
				{
					System.out.println("-list or -get or -set or -del is required.");
					return;
				}

				if ((list && (get || set || del)) || (get && (list || set || del)) || (set && (get || list || del)) || (del && (get || set || list)))
				{
					System.out.println("Use only one -list or -get or -set or -del parameter.");
					return;
				}

				if (vaultfile.isEmpty())
				{
					System.out.println("vault-file is required.");
					return;
				}

				vault.setPassword(vaultpass);

				boolean openVault = false;
				boolean saveVault = set || del;
				if (set)
				{
					File f = new File(vaultfile);
					openVault = f.exists();
				}
				else
					openVault = true;

				if (openVault)
				{
					vault.openFile(vaultfile);
				}


				if (list)
				{
					if (entry.isEmpty())
					{
						String entries = vault.listEntries();

						System.out.println("Entries: ");
						for (String entryName : entries.split(";"))
						{
							System.out.println(entryName);
						}
					}
					else
					{
						String fields = vault.listFields(entry, true);

						System.out.println("Fields in \"" + entry + "\": ");
						for (String fieldName : fields.split(";"))
						{
							System.out.println(fieldName);
						}
					}
				}
				else
					if (get)
					{
						if (entry.isEmpty())
						{
							System.out.println("-entry is required.");
							return;
						}

						if (field.isEmpty())
						{
							System.out.println("-field is required.");
							return;
						}

						vault.setEntryPassword(entrypass);

						value = vault.getEntryValueStr(entry, field);
						System.out.println("Value: " + value);
					}
					else
						if (set)
						{
							if (entry.isEmpty())
							{
								System.out.println("-entry is required.");
								return;
							}

							if (field.isEmpty()) {
								vault.addEntry(entry);
								System.out.println("Entry \"" + entry + "\" successfully added.");
							}
							else {
								vault.setEntryPassword(entrypass);

								vault.setEntryValueStr(entry, field, value, !entrypass.isEmpty());
								System.out.println("Field \"" + field + "\" successfully added/modify.");
							}
						}
						else // del
						{
							if (entry.isEmpty())
							{
								System.out.println("-entry is required.");
								return;
							}

							if (field.isEmpty()) {
								vault.removeEntry(entry);
								System.out.println("Entry \"" + entry + "\" successfully removed.");
							}
							else {
								vault.removeField(entry, field);
								System.out.println("Field \"" + field + "\" successfully removed.");
							}
						}

				if (saveVault)
				{
					vault.saveFile(vaultfile);
				}
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



