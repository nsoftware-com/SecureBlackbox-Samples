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

public class pkcs11certificatestorage extends ConsoleDemo {
	private static String optval(String[] args, String option) {
		for (int x = 0; x < args.length - 1; x++) {
			if (args[x].equalsIgnoreCase(option)) {
				return args[x + 1];
			}
		}
		return "";
	}

	private static boolean optext(String[] args, String option) {
		for (int x = 0; x < args.length; x++) {
			if (args[x].equalsIgnoreCase(option)) {
				return true;
			}
		}
		return false;
	}

	private static void displayHelp(String errMes) {
		System.out.println(
				"NAME\n" +
						"  pkcs11certificatestorage -- SecureBlackbox CertificateStorage Demo Application\n\n" +
						"SYNOPSIS\n" +
						"  pkcs11certificatestorage <-storage driver_path> [-pin pin]\n\n" +
						"DESCRIPTION\n" +
						"  This sample illustrates the use of CertificateStorage component to access HSMs via PKCS11 interface. \n\n" +
						"  The options are as follows:\n\n" +
						"  -storage      A path to the pkcs11 driver file (Required).\n\n" +
						"  -pin          The user PIN for the device. If no PIN is provided, the sample won't be signing in.\n\n" +
						"EXAMPLES\n" +
						"  pkcs11certificatestorage -storage C:\\pkcs11\\pkcs11.dll -pin mypassword\n"
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
			CertificateStorage certstorage = new CertificateStorage();
			CertificateStorage certstorage_dop = new CertificateStorage();

			String storageFile = "";
			String pin = "";


			if (optext(args, "-storage")) {
				storageFile = optval(args, "-storage");
			} else {
				displayHelp("-storage is required.");
				return;
			}


			if (optext(args, "-pin")) {
				pin = optval(args, "-pin");
			}

			certstorage.open("pkcs11:///" + storageFile + "?slot=-1");

			String slots[] = certstorage.listStores().split("\\r?\\n");

			for (int i = 0; i < slots.length; i++) {
				String desc = slots[i];
				String active = certstorage.config("PKCS11SlotTokenPresent[" + i + "]");

				if (desc != "") {
					if (active == "True") {
						System.out.println(desc + ":");

						certstorage_dop.open("pkcs11://user:" + pin + "@/" + storageFile + "?slot=" + i);

						for (int j = 0; j < certstorage_dop.getCertificates().size(); j++) {
							System.out.println("	Subject: " + certstorage_dop.getCertificates().item(j).getSubject());
							System.out.println("	Issuer: " + certstorage_dop.getCertificates().item(j).getIssuer());
							System.out.println("	ValidFrom: " + certstorage_dop.getCertificates().item(j).getValidFrom());
							System.out.println("	ValidTo: " + certstorage_dop.getCertificates().item(j).getValidTo());
							System.out.println("	Key: " + certstorage_dop.getCertificates().item(j).getKeyAlgorithm() + " (" + certstorage_dop.getCertificates().item(j).getKeyBits() + ")");
							System.out.println("");
						}

						System.out.println("");
					} else {
						System.out.println(desc + ": No token ");
						System.out.println("");
					}
				}
			}

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



