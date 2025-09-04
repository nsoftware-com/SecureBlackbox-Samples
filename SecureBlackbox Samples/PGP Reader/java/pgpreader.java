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

import static secureblackbox.PGPSignature.*;

public class pgpreader extends ConsoleDemo {
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

	private static String translateSigValidity(int type) {
		switch (type) {
			case svtValid: return "Valid";
			case svtFailure: return "Failure";
			case svtCorrupted: return "Corrupted";
			case svtReferenceCorrupted: return "Reference Corrupted";
			case svtSignerNotFound: return "Signing key not found, unable to verify";
			default: return "Unknown";
		}
	}

	private static void displayHelp(String errMes) {
		System.out.println(
				"NAME\n" +
				"  pgpreader -- SecureBlackbox PGPReader Demo Application\n\n" +
				"SYNOPSIS\n" +
				"  pgpreader <-input input_file> <-output output_file> <-pubkey public_key_file> <-seckey secret_key_file>\n" +
				"            <-keypass keys_password> <-pass encryption_password> \n\n" +
				"DESCRIPTION\n" +
				"  PGPReader demonstrates the usage of PGPReader from SecureBlackbox.\n" +
				"  Used to decrypted and verify OpenPGP-compliant files. \n\n" +
				"  The options are as follows:\n\n" +
				"  -input        An input file to decrypt and verify (Required).\n\n" +
				"  -output       Where the decrypted file will be saved (Required).\n\n" +
				"  -pubkey       The public key used to verify file (Required).\n\n" +
				"  -seckey       The secret (private) key used to decrypt file (Required).\n\n" +
				"  -keypass      The password for the keys (Required).\n\n" +
				"  -pass         The password for decryption (Required).\n"
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

	private static String keypass = "";

	public static void main(String[] args) {
		if (args.length == 0) {
			displayHelp("");
			return;
		}

		PGPReader reader = new PGPReader();
		PGPKeyring keyring = new PGPKeyring();


		try {
			reader.addPGPReaderEventListener(new PGPReaderEventListener() {
				@Override
				public void encryptionInfo(PGPReaderEncryptionInfoEvent e) {

				}

				@Override
				public void error(PGPReaderErrorEvent e) {

				}

				@Override
				public void externalDecrypt(PGPReaderExternalDecryptEvent e) {

				}

				@Override
				public void fileExtractionFinish(PGPReaderFileExtractionFinishEvent e) {

				}

				@Override
				public void fileExtractionStart(PGPReaderFileExtractionStartEvent e) {

				}

				@Override
				public void keyPassphraseNeeded(PGPReaderKeyPassphraseNeededEvent e) {
					e.passphrase = keypass;
				}

				@Override
				public void multipleFilesFound(PGPReaderMultipleFilesFoundEvent e) {

				}

				@Override
				public void notification(PGPReaderNotificationEvent e) {

				}

				@Override
				public void passphraseNeeded(PGPReaderPassphraseNeededEvent e) {

				}

				@Override
				public void progress(PGPReaderProgressEvent e) {

				}

				@Override
				public void signed(PGPReaderSignedEvent e) {

				}

				@Override
				public void supercoreIntercept(PGPReaderSupercoreInterceptEvent e) {

				}
			});

			if (optext(args, "-input")) {
				reader.setInputFile(optval(args, "-input"));
			} else {
				displayHelp("-input is required.");
				return;
			}

			if (optext(args, "-output")) {
				reader.setOutputFile(optval(args, "-output"));
			} else {
				displayHelp("-output is required.");
				return;
			}

			if (optext(args, "-pass")) {
				reader.setPassphrase(optval(args, "-pass"));
			} else {
				displayHelp("-pass is required.");
				return;
			}

			if (optext(args, "-keypass")) {
				keypass = optval(args, "-keypass");
			} else {
				displayHelp("-keypass is required.");
				return;
			}

			if (optext(args, "-pubkey")) {
				keyring.importFromFile(optval(args, "-pubkey"));
			} else {
				displayHelp("-pubkey is required.");
				return;
			}

			if (optext(args, "-seckey")) {
				keyring.importFromFile(optval(args, "-seckey"));
			} else {
				displayHelp("-seckey is required.");
				return;
			}

			for (int i = 0; i < keyring.getKeys().size(); i++) {
				reader.getVerifyingKeys().add(keyring.getKeys().item(i));
				if (keyring.getKeys().item(i).getIsSecret())
				{
					reader.getDecryptingKeys().add(keyring.getKeys().item(i));
				}
			}

			reader.decryptAndVerify();

			System.out.println("Signatures:");
			for (int x = 0; x < reader.getSignatures().size(); x++) {
				String username = "No name";
				for (int y = 0; y < keyring.getKeys().size(); y++) {
					if (keyring.getKeys().item(y).getIsPublic() && !keyring.getKeys().item(y).getIsSubkey() &&
							keyring.getKeys().item(y).getKeyID().equals(reader.getSignatures().item(x).getSignerKeyID())) {
						username = keyring.getKeys().item(y).getUsername();
						break;
					}
				}
				System.out.println("	" + username + " - " + translateSigValidity(reader.getSignatures().item(x).getValidity()));
			}

			System.out.println("The file were decrypted successfully.\n");

			confirmExit();
		} catch (Exception ex) {
			ConsoleDemo.displayError(ex);
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



