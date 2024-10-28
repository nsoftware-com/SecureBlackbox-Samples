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

public class pdfsignerexternal extends ConsoleDemo {
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

	public static String byteToHex(byte num) {
		char[] hexDigits = new char[2];
		hexDigits[0] = Character.forDigit((num >> 4) & 0xF, 16);
		hexDigits[1] = Character.forDigit((num & 0xF), 16);
		return new String(hexDigits);
	}

	public static String encodeHexString(byte[] byteArray) {
		StringBuffer hexStringBuffer = new StringBuffer();
		for (int i = 0; i < byteArray.length; i++) {
			hexStringBuffer.append(byteToHex(byteArray[i]));
		}
		return hexStringBuffer.toString();
	}

	private static int toDigit(char hexChar) {
		int digit = Character.digit(hexChar, 16);
		if (digit == -1) {
			throw new IllegalArgumentException(
					"Invalid Hexadecimal Character: " + hexChar);
		}
		return digit;
	}

	public static byte hexToByte(String hexString) {
		int firstDigit = toDigit(hexString.charAt(0));
		int secondDigit = toDigit(hexString.charAt(1));
		return (byte) ((firstDigit << 4) + secondDigit);
	}

	public static byte[] decodeHexString(String hexString) {
		if (hexString.length() % 2 == 1) {
			throw new IllegalArgumentException(
					"Invalid hexadecimal String supplied.");
		}

		byte[] bytes = new byte[hexString.length() / 2];
		for (int i = 0; i < hexString.length(); i += 2) {
			bytes[i / 2] = hexToByte(hexString.substring(i, i + 2));
		}
		return bytes;
	}

	private static void displayHelp(String errMes) {
		System.out.println(
				"NAME\n" +
						"  pdfsignerexternal -- SecureBlackbox PDFSigner Demo Application\n\n" +
						"SYNOPSIS\n" +
						"  pdfsignerexternal <-input input_file> <-output output_file> <-key key_file> <-cert certificate_file>\n" +
						"                 [-certpass certificate_password] [-level sig_level]\n\n" +
						"DESCRIPTION\n" +
						"  This sample illustrates the use of PDFSigner component for signing PDF documents.\n\n" +
						"  The options are as follows:\n\n" +
						"  -input        An input file to sign (Required).\n\n" +
						"  -output       Where the signed file will be saved (Required).\n\n" +
						"  -key          The key file to be imported (Required).\n\n" +
						"  -cert         The certificate used to sign files (Required).\n\n" +
						"  -certpass     The password for the signing certificate.\n\n" +
						"  -level        The level for PAdES signatures. Enter the corresponding number. Valid values:\n\n" +
        				        "                  0  - PASL_UNKNOWN\n" +
        				        "                  1  - PASL_GENERIC\n" +
				                "                  2  - PASL_BASELINE_B\n" +
				                "                  3  - PASL_BASELINE_T\n" +
				                "                  4  - PASL_BASELINE_LT\n" +
				                "                  5  - PASL_BASELINE_LTA\n" +
				                "                  6  - PASL_BES\n" +
				                "                  7  - PASL_EPES\n" +
				                "                  8  - PASL_LTV\n\n" +
						"EXAMPLES\n" +
						"  pdfsignerexternal -input C:\\helloworld.pdf -output C:\\sign.pdf -key test.key -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
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

	public static String keyFile = "";

	public static void main(String[] args) {
		if (args.length == 0) {
			displayHelp("");
			return;
		}

		PDFSigner signer = new PDFSigner();
		CertificateManager cm = new CertificateManager();
		try {
			signer.addPDFSignerEventListener(new PDFSignerEventListener() {
				@Override
				public void chainElementDownload(PDFSignerChainElementDownloadEvent pdfsignerChainElementDownloadEvent) {

				}

				@Override
				public void chainElementNeeded(PDFSignerChainElementNeededEvent pdfsignerChainElementNeededEvent) {

				}

				@Override
				public void chainValidated(PDFSignerChainValidatedEvent pdfsignerChainValidatedEvent) {

				}

				@Override
				public void chainValidationProgress(PDFSignerChainValidationProgressEvent pdfsignerChainValidationProgressEvent) {

				}

				@Override
				public void documentLoaded(PDFSignerDocumentLoadedEvent pdfsignerDocumentLoadedEvent) {

				}

				@Override
				public void encrypted(PDFSignerEncryptedEvent pdfsignerEncryptedEvent) {

				}

				@Override
				public void error(PDFSignerErrorEvent pdfsignerErrorEvent) {

				}

				@Override
				public void externalDecrypt(PDFSignerExternalDecryptEvent pdfsignerExternalDecryptEvent) {

				}

				@Override
				public void externalSign(PDFSignerExternalSignEvent e) {
					PublicKeyCrypto crypto = new PublicKeyCrypto();
					try {
						CryptoKeyManager keymanager = new CryptoKeyManager();

						keymanager.importFromFile(keyFile, 3, "", "", e.pars, 0, "");

						crypto.setKey(keymanager.getKey());
					} catch (Exception ex) {
						System.out.println("Cannot load key!");
					}

					try {
						crypto.setHashAlgorithm(e.hashAlgorithm);
						crypto.setInputIsHash(true);
						crypto.setSchemeParams(e.pars);

						byte[] inBuf = decodeHexString(e.data);
						byte[] outBuf = crypto.sign(inBuf, true);

						e.signedData = encodeHexString(outBuf);
					} catch (Exception ex) {
						System.out.println("Cannot signing data!");
					}
				}

				@Override
				public void notification(PDFSignerNotificationEvent pdfsignerNotificationEvent) {

				}

				@Override
				public void preRenderWidget(PDFSignerPreRenderWidgetEvent pdfsignerPreRenderWidgetEvent) {

				}

				@Override
				public void signatureFound(PDFSignerSignatureFoundEvent pdfsignerSignatureFoundEvent) {

				}

				@Override
				public void signatureValidated(PDFSignerSignatureValidatedEvent pdfsignerSignatureValidatedEvent) {

				}

				@Override
				public void timestampFound(PDFSignerTimestampFoundEvent pdfsignerTimestampFoundEvent) {

				}

				@Override
				public void timestampRequest(PDFSignerTimestampRequestEvent pdfsignerTimestampRequestEvent) {

				}

				@Override
				public void timestampValidated(PDFSignerTimestampValidatedEvent pdfsignerTimestampValidatedEvent) {

				}

				@Override
				public void TLSCertNeeded(PDFSignerTLSCertNeededEvent pdfsignerTLSCertNeededEvent) {

				}

				@Override
				public void TLSCertValidate(PDFSignerTLSCertValidateEvent pdfsignerTLSCertValidateEvent) {

				}

				@Override
				public void TLSEstablished(PDFSignerTLSEstablishedEvent pdfsignerTLSEstablishedEvent) {

				}

				@Override
				public void TLSHandshake(PDFSignerTLSHandshakeEvent pdfsignerTLSHandshakeEvent) {

				}

				@Override
				public void TLSShutdown(PDFSignerTLSShutdownEvent pdfsignerTLSShutdownEvent) {

				}

				@Override
				public void supercoreIntercept(PDFSignerSupercoreInterceptEvent pdfsignerSupercoreInterceptEvent) {

				}
			});

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

			if (optext(args, "-key")) {
				keyFile = optval(args, "-key");
			} else {
				displayHelp("-key is required.");
				return;
			}

			if (optext(args, "-cert")) {
				cm.importFromFile(optval(args, "-cert"), optval(args, "-certpass"));
				signer.setSigningCertificate(cm.getCertificate());
			} else {
				displayHelp("-cert is required.");
				return;
			}

			signer.getNewSignature().setAuthorName("test demo author");
			signer.getNewSignature().setReason("test demo reason");
			signer.getWidget().setInvisible(false);

			signer.setIgnoreChainValidationErrors(true);
			signer.getExternalCrypto().setMode(ExternalCrypto.ecmGeneric);

			signer.signExternal();

			System.out.println("PDF file successfully signed.\n");

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



