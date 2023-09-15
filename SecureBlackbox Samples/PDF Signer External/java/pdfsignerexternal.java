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

public class pdfsignerexternal extends ConsoleDemo {

	public static String keyFile = "";

	public static Certificate loadCertificate(String file, String password)
	{
		Certificate cert = null;

		if (file.length() > 0)
		{
			try
			{
				Certificatemanager certmanager = new Certificatemanager();

				certmanager.importFromFile(file, password);

				cert = certmanager.getCertificate();
			}
			catch (Exception e)
			{
				System.out.println("Cannot load certificate!");
			}
		}

		return cert;
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
		if(digit == -1) {
			throw new IllegalArgumentException(
					"Invalid Hexadecimal Character: "+ hexChar);
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

	public static void main(String[] args) {
		if (args.length < 8) {
			System.out.println("usage: pdfsignerexternal [-input input_file] [-output output_file] [-cert certificate_file] [-certpass certificate_password] [-key key_file]");
			System.out.println("Options: ");
			System.out.println("  -input        An input PDF file to sign (Required).");
			System.out.println("  -output       Where the signing PDF file will be saved (Required).");
			System.out.println("  -cert         The certificate used to sign file. (Required)");
			System.out.println("  -certpass     The password for the certificate.");
			System.out.println("  -key          The key used to sign file. (Required)");
			System.out.println("\r\nExample: pdfsignerexternal -input C:\\pdf\\myfile.pdf -output C:\\pdf\\mysignedfile.pdf -cert C:\\certs\\mycert.pfx -key C:\\keys\\mykey.pem");
		} else {
			try {
				Pdfsigner signer = new Pdfsigner();
				System.out.println("***************************************************************************************************");
				System.out.println("* This sample illustrates the use of PDFSigner component for external signing PDF documents. *");
				System.out.println("***************************************************************************************************\n");

				String certFile = "";
				String certPass = "";

				signer.addPdfsignerEventListener(new PdfsignerEventListener() {
					@Override
					public void chainElementDownload(PdfsignerChainElementDownloadEvent pdfsignerChainElementDownloadEvent) {

					}

					@Override
					public void chainElementNeeded(PdfsignerChainElementNeededEvent pdfsignerChainElementNeededEvent) {

					}

					@Override
					public void chainValidated(PdfsignerChainValidatedEvent pdfsignerChainValidatedEvent) {

					}

					@Override
					public void chainValidationProgress(PdfsignerChainValidationProgressEvent pdfsignerChainValidationProgressEvent) {

					}

					@Override
					public void documentLoaded(PdfsignerDocumentLoadedEvent pdfsignerDocumentLoadedEvent) {

					}

					@Override
					public void encrypted(PdfsignerEncryptedEvent pdfsignerEncryptedEvent) {

					}

					@Override
					public void error(PdfsignerErrorEvent pdfsignerErrorEvent) {

					}

					@Override
					public void externalDecrypt(PdfsignerExternalDecryptEvent pdfsignerExternalDecryptEvent) {

					}

					@Override
					public void externalSign(PdfsignerExternalSignEvent e) {
						Publickeycrypto crypto = new Publickeycrypto();
						try
						{
							Cryptokeymanager keymanager = new Cryptokeymanager();

							keymanager.importFromFile(keyFile, 3, "", "", e.pars, 0);

							crypto.setKey(keymanager.getKey());
						}
						catch (Exception ex)
						{
							System.out.println("Cannot load key!");
						}

						try
						{
							crypto.setHashAlgorithm(e.hashAlgorithm);
							crypto.setInputIsHash(true);
							crypto.setSchemeParams(e.pars);

							byte[] inBuf = decodeHexString(e.data);
							byte[] outBuf = crypto.sign(inBuf, true);

							e.signedData = encodeHexString(outBuf);
						}
						catch (Exception ex)
						{
							System.out.println("Cannot signing data!");
						}
					}

					@Override
					public void notification(PdfsignerNotificationEvent pdfsignerNotificationEvent) {

					}

					@Override
					public void preRenderWidget(PdfsignerPreRenderWidgetEvent pdfsignerPreRenderWidgetEvent) {

					}

					@Override
					public void signatureFound(PdfsignerSignatureFoundEvent pdfsignerSignatureFoundEvent) {

					}

					@Override
					public void signatureValidated(PdfsignerSignatureValidatedEvent pdfsignerSignatureValidatedEvent) {

					}

					@Override
					public void timestampFound(PdfsignerTimestampFoundEvent pdfsignerTimestampFoundEvent) {

					}

					@Override
					public void timestampRequest(PdfsignerTimestampRequestEvent pdfsignerTimestampRequestEvent) {

					}

					@Override
					public void timestampValidated(PdfsignerTimestampValidatedEvent pdfsignerTimestampValidatedEvent) {

					}

					@Override
					public void TLSCertNeeded(PdfsignerTLSCertNeededEvent pdfsignerTLSCertNeededEvent) {

					}

					@Override
					public void TLSCertValidate(PdfsignerTLSCertValidateEvent pdfsignerTLSCertValidateEvent) {

					}

					@Override
					public void TLSEstablished(PdfsignerTLSEstablishedEvent pdfsignerTLSEstablishedEvent) {

					}

					@Override
					public void TLSHandshake(PdfsignerTLSHandshakeEvent pdfsignerTLSHandshakeEvent) {

					}

					@Override
					public void TLSShutdown(PdfsignerTLSShutdownEvent pdfsignerTLSShutdownEvent) {

					}

					@Override
					public void supercoreIntercept(PdfsignerSupercoreInterceptEvent pdfsignerSupercoreInterceptEvent) {

					}
				});

				for (int i = 0; i < args.length; i++) {
					if (args[i].startsWith("-")) {
						if (args[i].equals("-input"))
							signer.setInputFile(args[i + 1]); // args[i+1] corresponds to the value of argument [i]
						if (args[i].equals("-output"))
							signer.setOutputFile(args[i + 1]);
						if (args[i].equals("-cert"))
							certFile = args[i + 1];
						if (args[i].equals("-certpass"))
							certPass = args[i + 1];
						if (args[i].equals("-key"))
							keyFile = args[i + 1];
					}
				}

				if (certFile.isEmpty())
				{
					System.out.println("-cert is required");
					return;
				}

				if (keyFile.isEmpty())
				{
					System.out.println("-key is required");
					return;
				}

				signer.setSigningCertificate(loadCertificate(certFile, certPass));

				signer.getNewSignature().setAuthorName("test demo author");
				signer.getNewSignature().setReason("test demo reason");
				signer.getWidget().setInvisible(false);

				signer.setIgnoreChainValidationErrors(true);
				signer.getExternalCrypto().setMode(ExternalCrypto.ecmGeneric);

				signer.signExternal();

				System.out.println("PDF file successfully signed");
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



