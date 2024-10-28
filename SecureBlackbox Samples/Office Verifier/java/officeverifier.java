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

import static secureblackbox.OfficeSignature.*;

public class officeverifier extends ConsoleDemo {
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
				"  officeverifier -- SecureBlackbox OfficeVerifier Demo Application\n\n" +
				"SYNOPSIS\n" +
				"  officeverifier <-input input_file> [-cert certificate_file] [-certpass certificate_password] [-checkrev] [-ignoreerrors] [-offline]\n" +
				"DESCRIPTION\n" +
				"  OfficeVerifier demonstrates the usage of OfficeVerifier from SecureBlackbox.\n" +
				"  Used to verify the signature.\n\n" +
				"  The options are as follows:\n\n" +
				"  -input        An input file to verify (Required).\n\n" +
				"  -cert         The certificate used to sign files.\n\n" +
				"  -certpass     The password for the signing certificate.\n\n" +
				"  -checkrev      Whether certificate revocation information should be checked.\n\n" +
				"  -ignoreerrors  Whether to ignore chain validation errors.\n\n" +
				"  -offline       Whether offline mode be used.\n\n" +
				"EXAMPLES\n" +
				"  officeverifier -input C:\\myfile.doc -output C:\\sign.doc -offline\n\n" +
				"  officeverifier -input C:\\myfile.doc -output C:\\sign.doc -checkrev -ignoreerrors\n"
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

	private static String translateSigType(int value) {
		switch (value) {
			case ostBinaryCryptoAPI: return "BinaryCryptoAPI";
			case ostBinaryXML: return "BinaryXML";
			case ostOpenXML: return "OpenXML";
			case ostOpenXPS: return "OpenXPS";
			case ostOpenDocument: return "OpenOffice";
			default: return "Unknown";
		}
	}

	private static String translateDocSig(boolean value) {
		return value ? "Document content is signed" : "Document content is partially signed";
	}

	private static String translateCore(boolean value) {
		return value ? "Document properties are signed" : "Document properties are not signed";
	}

	private static String translateOrigSig(boolean value) {
		return value ? "Signature origin is signed" : "Signature origin is not signed";
	}

	private static String translateSigValidity(int value) {
		switch (value) {
			case svtValid: return "Valid";
			case svtCorrupted: return "Corrupted";
			case svtSignerNotFound: return "Signer not found";
			case svtFailure: return "Failure";
			default: return "Unknown";
		}
	}

	private static String translateChainValidity(int value) {
		switch (value) {
			case cvtValid: return "Valid";
			case cvtValidButUntrusted: return "ValidButUntrusted";
			case cvtInvalid: return "Invalid";
			case cvtCantBeEstablished: return "CantBeEstablished";
			default: return "Unknown";
		}
	}

	public static void main(String[] args) {
		if (args.length == 0) {
			displayHelp("");
			return;
		}

		try {
			OfficeVerifier verifier = new OfficeVerifier();

			if (optext(args, "-input")) {
				verifier.setInputFile(optval(args, "-input"));
			} else {
				displayHelp("-input is required.");
				return;
			}

			if (optext(args, "-cert")) {
				CertificateManager cm = new CertificateManager();
				cm.importFromFile(optval(args, "-cert"), optval(args, "-certpass"));
				verifier.getKnownCertificates().add(cm.getCertificate());
			}

			// Additional options
			if (optext(args, "-checkrev")) {
				verifier.setRevocationCheck(OfficeVerifier.crcAuto);
			} else {
				verifier.setRevocationCheck(OfficeVerifier.crcNone);
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

			System.out.println("There are " + verifier.getSignatures().size() + " signatures in this file.");
			for (int x = 0; x < verifier.getSignatures().size(); x++) {
				System.out.println("Signature #" + (x + 1));
				System.out.println("  Signature type: " + translateSigType(verifier.getSignatures().item(x).getSignatureType()));
				System.out.println("  " + translateDocSig(verifier.getSignatures().item(x).getDocumentSigned()));
				System.out.println("  " + translateCore(verifier.getSignatures().item(x).getCorePropertiesSigned()));
				System.out.println("  " + translateOrigSig(verifier.getSignatures().item(x).getSignatureOriginSigned()));
				System.out.println("  Signature Time: " + verifier.getSignatures().item(x).getClaimedSigningTime());
				System.out.println("  Signature Validation Result: " + translateSigValidity(verifier.getSignatures().item(x).getSignatureValidationResult()));
				System.out.println("  Chain Validation Result: " + translateChainValidity(verifier.getSignatures().item(x).getChainValidationResult()) + "\n");
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



