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

public class authenticodesigner extends ConsoleDemo {
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
				"  authenticodesigner -- SecureBlackbox AuthenticodeSigner Demo Application\n\n" +
				"SYNOPSIS\n" +
				"  authenticodesigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password]\n" +
				"             [-sigurl sigurl] [-hashalg hashalg] [-individual] [-remove] [-tsserver timestamp_server]\n\n" +
				"DESCRIPTION\n" +
				"  AuthenticodeSigner demonstrates the usage of AuthenticodeSigner from SecureBlackbox.\n" +
				"  Used to sign EXE and DLL files.\n\n" +
				"  The options are as follows:\n\n" +
				"  -input        An input file to sign (Required).\n\n" +
				"  -output       Where the signed file will be saved (Required).\n\n" +
				"  -cert         The certificate used to sign files (Required).\n\n" +
				"  -certpass     The password for the signing certificate.\n\n" +
				"  -sigurl       The signature URL.\n\n" +
				"  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224, MD5\n\n" +
				"  -individual   Whether to use individual signatures.\n\n" +
				"  -tsserver     A timestamp server to use during signing.\n\n" +
				"  -remove       Whether to remove existing signature.\n\n" +
				"EXAMPLES\n" +
				"  authenticodesigner -input C:\\helloworld.exe -output C:\\myexe.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n" +
				"  authenticodesigner -input C:\\helloworld.dll -output C:\\mydll.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n" +
				"             -hashalg SHA256 -individual -remove -tsserver http://timestamp.wosign.com\n"
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
			AuthenticodeSigner signer = new AuthenticodeSigner();
			CertificateManager certmanager = new CertificateManager();

			if (optext(args, "-input")) {
				signer.setInputFile(optval(args, "-input"));
			} else {
				displayHelp("-input is required.");
				return;
			}

			if (optext(args, "-output")) {
				signer.setOutputFile(optval(args, "-output"));
			} else {
				displayHelp("-output is required.");
				return;
			}

			if (optext(args, "-cert")) {
				certmanager.importFromFile(optval(args, "-cert"), optval(args, "-certpass"));
				signer.setSigningCertificate(certmanager.getCertificate());
			} else {
				displayHelp("-cert is required.");
				return;
			}

			if (optext(args, "-sigurl")) {
				signer.setSignatureURL(optval(args, "-sigurl"));
			}

			if (optext(args, "-hashalg")) {
				signer.setHashAlgorithm(optval(args, "-hashalg"));
			}

			if (optext(args, "-tsserver")) {
				signer.setTimestampServer(optval(args, "-tsserver"));
			}

			if (optext(args, "-tsserver")) {
				signer.setStatementType(AuthenticodeSigner.acsIndividual);
			}
			else
			{
				signer.setStatementType(AuthenticodeSigner.acsCommercial);
			}

			if (optext(args, "-remove")) {
				signer.setRemoveExistingSignatures(true);
			}

			signer.sign();

			System.out.println("The file has been successfully signed.\n");

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



