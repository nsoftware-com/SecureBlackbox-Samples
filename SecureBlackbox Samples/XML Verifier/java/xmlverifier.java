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

public class xmlverifier extends ConsoleDemo {
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
				"  xmlverifier -- SecureBlackbox XMLVerifier Demo Application\n\n" +
				"SYNOPSIS\n" +
				"  xmlverifier <-input input_file> [-datafile original_data] [-cert certificate_file] [-certpass certificate_password] \n" +
				"             [-detached] [-showrefs]\n" +
				"DESCRIPTION\n" +
				"  XMLVerifier demonstrates the usage of XMLVerifier from SecureBlackbox.\n" +
				"  Used to verify an XML Signature from an XML file.\n\n" +
				"  The options are as follows:\n\n" +
				"  -input        A signature to verify (Required). If the signature is detached, this will take\n" +
				"                the signature file and -datafile will take the original data.\n\n" +
				"  -cert         The certificate used to verify the signature. Required if no key is included in the signature.\n\n" +
				"  -certpass     The password for the certificate.\n\n" +
				"  -detached     Whether the signature is detached. Use -datafile to specify the original data.\n\n" +
				"  -datafile     The original data.\n\n" +
				"  -showrefs     Whether to display detailed results of reference verification.\n\n" +
				"EXAMPLES\n" +
				"  xmlverifier -input C:\\xml\\mysigned.xml\n" +
				"  xmlverifier -input C:\\xml\\mysigned.xml -detached -datafile C:\\xml\\my.xml\n" +
				"  xmlverifier -input C:\\xml\\mysigned.xml -cert C:\\certs\\mycert.pfx -certpass test\n" +
				"  xmlverifier -input C:\\xml\\mysigned.xml -showrefs\n"
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

	private static String translateValidationResult(int res) {
		switch (res) {
			case XAdESSignature.svtValid:
				return "The signature is valid.";
			case XAdESSignature.svtCorrupted:
				return "The signature is corrupted.";
			case XAdESSignature.svtSignerNotFound:
				return "Failed to acquire the signing certificate. The signature cannot be validated.";
			case XAdESSignature.svtFailure:
				return "General failure.";
			case XAdESSignature.svtReferenceCorrupted:
				return "The signature has invalid reference(s).";
			default:
				return "Signature validity is unknown.";
		}
	}

	private static boolean showrefs = false;

	public static void main(String[] args) {
		if (args.length == 0) {
			displayHelp("");
			return;
		}

		XMLVerifier verifier = new XMLVerifier();
		try {
			verifier.addXMLVerifierEventListener(new XMLVerifierEventListener() {
				@Override
				public void documentLoaded(XMLVerifierDocumentLoadedEvent e) {

				}

				@Override
				public void error(XMLVerifierErrorEvent e) {

				}

				@Override
				public void notification(XMLVerifierNotificationEvent e) {

				}

				@Override
				public void referenceValidated(XMLVerifierReferenceValidatedEvent e) {
					if (showrefs) {
						String valid = "false";
						if (e.digestValid)
							valid = "true";
						System.out.println(e.ID + "	" + e.URI + "	" + e.refType + "	" + valid);
					}
				}

				@Override
				public void resolveReference(XMLVerifierResolveReferenceEvent e) {

				}

				@Override
				public void signatureFound(XMLVerifierSignatureFoundEvent e) {

				}

				@Override
				public void signatureValidated(XMLVerifierSignatureValidatedEvent e) {

				}

				@Override
				public void supercoreIntercept(XMLVerifierSupercoreInterceptEvent e) {

				}
			});

			if (optext(args, "-input")) {
				verifier.setInputFile(optval(args, "-input"));
			} else {
				displayHelp("-input is required.");
				return;
			}

			boolean detached = false;
			if (optext(args, "-detached")) {
				detached = true;

				if (optext(args, "-data")) {
					verifier.setDataFile(optval(args, "-data"));
					verifier.setDataType(XAdESVerifier.cxdtBinary);
					verifier.setDataURI("filename.txt"); // use real name of the input
				} else {
					displayHelp("-data is required if -detached is used.");
					return;
				}
			}

			if (optext(args, "-cert")) {
				CertificateManager cm = new CertificateManager();
				cm.importFromFile(optval(args, "-cert"), optval(args, "-certpass"));
				verifier.getKnownCertificates().add(cm.getCertificate());
			}

			if (optext(args, "-showrefs")) {
				showrefs = true;
				System.out.println(
						"ID URI RefType DigestValid?\n" +
								"---------------------"
				);
			}

			if (detached)
			{
				verifier.verifyDetached();
			}
			else
			{
				verifier.verify();
			}

			System.out.println("\nVerification complete.\n");
			for (int i = 0; i < verifier.getSignatures().size(); i++)
			{
				System.out.println("Signature #" + (i + 1));
				System.out.println("  Validation Result: " + verifier.getSignatures().item(i).getSignatureValidationResult() + ", " + translateValidationResult(verifier.getSignatures().item(i).getSignatureValidationResult()));
			}

			System.out.println();
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



