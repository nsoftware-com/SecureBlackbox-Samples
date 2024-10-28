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

import static secureblackbox.ASiCSignature.*;

public class jadesverifier extends ConsoleDemo {
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
				"  jadesverifier -- SecureBlackbox JAdESVerifier Demo Application\n\n" +
				"SYNOPSIS\n" +
				"  jadesverifier <-input input_file> [-data data_file] [-checkrev] [-ignoreerrors] [-offline]\n\n" +
				"DESCRIPTION\n" +
				"  This sample illustrates the use of JAdESVerifier component for validating JAdES signatures.\n\n" +
				"  The options are as follows:\n\n" +
				"  -input         An input file to verify (Required).\n\n" +
				"  -data          The payload to be validated (for detached signatures).\n\n" +
				"  -checkrev      Whether certificate revocation information should be checked.\n\n" +
				"  -ignoreerrors  Whether to ignore chain validation errors.\n\n" +
				"  -offline       Whether offline mode be used.\n\n" +
				"EXAMPLES\n" +
				"  jadesverifier -input C:\\mydll.scs -data C:\\helloworld.dll -checkrev -ignoreerrors\n"
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

	private static void printJAdESSignature(int idx, JAdESVerifier verifier) {
		System.out.println("Signature " + (idx + 1));
		System.out.println("  Timestamp:         " + verifier.getSignatures().item(idx).getValidatedSigningTime());
		System.out.print("  Validation Result: " + verifier.getSignatures().item(idx).getSignatureValidationResult() + ", ");

		switch (verifier.getSignatures().item(idx).getSignatureValidationResult()) {
			case svtValid:
				System.out.println("The signature is valid.");
				break;
			case svtCorrupted:
				System.out.println("The signature is corrupted.");
				break;
			case svtSignerNotFound:
				System.out.println("Failed to acquire the signing certificate. The signature cannot be validated.");
				break;
			case svtFailure:
				System.out.println("General failure.");
				break;
			default:
				System.out.println("Signature validity is unknown.");
				break;
		}

		System.out.println("  Chain Result:      " + verifier.getSignatures().item(idx).getChainValidationResult());
		System.out.println();
	}

	public static void main(String[] args) {
		if (args.length == 0) {
			displayHelp("");
			return;
		}

		JAdESVerifier verifier = new JAdESVerifier();

		try {
			if (optext(args, "-input")) {
				verifier.setInputFile(optval(args, "-input"));
			} else {
				displayHelp("-input is required.");
				return;
			}

			if (optext(args, "-data")) {
				verifier.setDataFile(optval(args, "-data"));
			}

			if (optext(args, "-checkrev")) {
				verifier.setRevocationCheck(JAdESVerifier.crcAuto);
			} else {
				verifier.setRevocationCheck(JAdESVerifier.crcNone);
			}

			if (optext(args, "-ignoreerrors")) {
				verifier.setIgnoreChainValidationErrors(true);
			} else {
				verifier.setIgnoreChainValidationErrors(false);
			}

			if (optext(args, "-offline")) {
				verifier.setOfflineMode(true);
			} else {
				verifier.setOfflineMode(false);
			}

			verifier.verify();

			for (int i = 0; i < verifier.getSignatures().size(); i++) {
				printJAdESSignature(i, verifier);
			}

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



