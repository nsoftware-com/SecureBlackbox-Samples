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

public class soapverifier extends ConsoleDemo {
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
						"  soapverifier -- SecureBlackbox SOAPVerifier Demo Application\n\n" +
						"SYNOPSIS\n" +
						"  soapverifier <-input input_file> [-cert certificate_file] [-certpass certificate_password] [-showrefs]\n" +
						"DESCRIPTION\n" +
						"  SOAPVerifier demonstrates the usage of SOAPVerifier from SecureBlackbox.\n" +
						"  Used to verify the signature.\n\n" +
						"  The options are as follows:\n\n" +
						"  -input        An input file to verify (Required).\n\n" +
						"  -cert         The certificate used to sign files.\n\n" +
						"  -certpass     The password for the signing certificate.\n\n" +
						"  -showrefs     Whether to display detailed results of reference verification.\n"
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

	static boolean showrefs = false;

	public static void main(String[] args) {
		if (args.length == 0) {
			displayHelp("");
			return;
		}

		SOAPVerifier verifier = new SOAPVerifier();
		try {
			verifier.addSOAPVerifierEventListener(new SOAPVerifierEventListener() {
				@Override
				public void chainElementDownload(SOAPVerifierChainElementDownloadEvent soapverifierChainElementDownloadEvent) {

				}

				@Override
				public void chainElementNeeded(SOAPVerifierChainElementNeededEvent soapverifierChainElementNeededEvent) {

				}

				@Override
				public void chainElementStore(SOAPVerifierChainElementStoreEvent soapverifierChainElementStoreEvent) {

				}

				@Override
				public void chainValidated(SOAPVerifierChainValidatedEvent soapverifierChainValidatedEvent) {

				}

				@Override
				public void chainValidationProgress(SOAPVerifierChainValidationProgressEvent soapverifierChainValidationProgressEvent) {

				}

				@Override
				public void error(SOAPVerifierErrorEvent soapverifierErrorEvent) {

				}

				@Override
				public void messageLoaded(SOAPVerifierMessageLoadedEvent soapverifierMessageLoadedEvent) {

				}

				@Override
				public void notification(SOAPVerifierNotificationEvent soapverifierNotificationEvent) {

				}

				@Override
				public void referenceValidated(SOAPVerifierReferenceValidatedEvent e) {
					if (showrefs) {
						String valid = "false";
						if (e.digestValid)
							valid = "true";
						System.out.println(e.ID + "	" + e.URI + "	" + e.refType + "	" + valid);
					}
				}

				@Override
				public void resolveReference(SOAPVerifierResolveReferenceEvent soapverifierResolveReferenceEvent) {

				}

				@Override
				public void signatureFound(SOAPVerifierSignatureFoundEvent soapverifierSignatureFoundEvent) {

				}

				@Override
				public void signatureValidated(SOAPVerifierSignatureValidatedEvent soapverifierSignatureValidatedEvent) {

				}

				@Override
				public void timestampFound(SOAPVerifierTimestampFoundEvent soapverifierTimestampFoundEvent) {

				}

				@Override
				public void timestampValidated(SOAPVerifierTimestampValidatedEvent soapverifierTimestampValidatedEvent) {

				}

				@Override
				public void TLSCertNeeded(SOAPVerifierTLSCertNeededEvent soapverifierTLSCertNeededEvent) {

				}

				@Override
				public void TLSCertValidate(SOAPVerifierTLSCertValidateEvent soapverifierTLSCertValidateEvent) {

				}

				@Override
				public void TLSEstablished(SOAPVerifierTLSEstablishedEvent soapverifierTLSEstablishedEvent) {

				}

				@Override
				public void TLSHandshake(SOAPVerifierTLSHandshakeEvent soapverifierTLSHandshakeEvent) {

				}

				@Override
				public void TLSShutdown(SOAPVerifierTLSShutdownEvent soapverifierTLSShutdownEvent) {

				}

				@Override
				public void supercoreIntercept(SOAPVerifierSupercoreInterceptEvent soapverifierSupercoreInterceptEvent) {

				}
			});

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

			if (optext(args, "-showrefs")) {
				showrefs = true;
			}

			if (showrefs) {
				System.out.println("ID	URI	RefType	DigestValid");
			}

			verifier.verify();

			System.out.println("There are " + verifier.getSignatures().size() + " signatures in this file.\n");
			for (Integer i = 0; i < verifier.getSignatures().size(); i++) {
				SOAPSignature sig = verifier.getSignatures().item(i);
				System.out.println("Signature " + (i + 1) + "\n");
				System.out.println("  Claimed signing time: " + sig.getClaimedSigningTime() + "\n");

				String s = "";
				switch (sig.getSignatureValidationResult()) {
					case SOAPSignature.svtValid:
						s = "Valid";
						break;
					case SOAPSignature.svtCorrupted:
						s = "Corrupted";
						break;
					case SOAPSignature.svtSignerNotFound:
						s = "SignerNotFound";
						break;
					case SOAPSignature.svtFailure:
						s = "Failure";
						break;
					case SOAPSignature.svtReferenceCorrupted:
						s = "ReferenceCorrupted";
						break;
					default:
						s = "Unknown";
						break;
				}
				System.out.println("Signature validation result: " + s + "\n");

				s = "";
				switch (sig.getChainValidationResult()) {
					case SOAPSignature.cvtValid:
						s = "Valid";
						break;
					case SOAPSignature.cvtValidButUntrusted:
						s = "ValidButUntrusted";
						break;
					case SOAPSignature.cvtInvalid:
						s = "Invalid";
						break;
					case SOAPSignature.cvtCantBeEstablished:
						s = "CantBeEstablished";
						break;
					default:
						s = "Unknown";
						break;
				}
				System.out.println("Chain Validation Result: " + s + "\n");
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



