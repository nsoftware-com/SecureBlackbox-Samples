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
import static secureblackbox.Publickeycrypto.*;

public class jwsigner extends ConsoleDemo {

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

	public static void main(String[] args) {
		if (args.length < 5) {
			System.out.println("usage: jwsigner -s/-v [-input input_data] [-sig signature_data] [-cert certificate_file] [-certpass certificate_password] [-compact]");
			System.out.println("Options: ");
			System.out.println("  -s            Whether to sign input data.");
			System.out.println("  -v            Whether to verify signature data.");
			System.out.println("  -input        An input data to sign/verify (Required).");
			System.out.println("  -sig          An signature data to verify (Required to verify).");
			System.out.println("  -cert         The certificate used to encrypt file (Required).");
			System.out.println("  -certpass     The password for the certificate.");
			System.out.println("  -compact      Whether to use compact format");
			System.out.println("\r\nExample: jwsigner -s -input \"And now that you donâ€™t have to be perfect, you can be good.\" -cert C:\\\\certs\\\\mycert.pfx -certpass mypassword");
			System.out.println("             jwsigner -v -input \"And now that you donâ€™t have to be perfect, you can be good.\"");
			System.out.println("             -sig eyJhbGciOiJkaXIiLCJlbmMiOiJBMjU2Q0JDLUhTNTEyIn0..kuN2U -cert C:\\\\certs\\\\mycert.pfx -certpass mypassword -compact");
		} else {
			try {
				Publickeycrypto crypto = new Publickeycrypto();
				System.out.println("***************************************************************************************************");
				System.out.println("* This sample illustrates how to create a detached signature over a text string. *");
				System.out.println("***************************************************************************************************\n");

				boolean sign = false;
				boolean verify = false;
				String input = "";
				String sig = "";
				String certfile = "";
				String certpass = "";
				boolean compact = false;

				for (int i = 0; i < args.length; i++) {
					if (args[i].startsWith("-")) {
						if (args[i].equals("-s"))
							sign = true;
						if (args[i].equals("-v"))
							verify = true;
						if (args[i].equals("-input"))
							input = args[i + 1]; // args[i+1] corresponds to the value of argument [i]
						if (args[i].equals("-sig"))
							sig = args[i + 1];
						if (args[i].equals("-cert"))
							certfile = args[i + 1];
						if (args[i].equals("-certpass"))
							certpass = args[i + 1];
						if (args[i].equals("-compact"))
							compact = true;
					}
				}

				if (!(sign || verify))
				{
					System.out.println("-s or -v is required.");
					return;
				}

				if (sign && verify)
				{
					System.out.println("Use only one -s or -v parameter.");
					return;
				}

				if (input.isEmpty())
				{
					System.out.println("-input is required.");
					return;
				}

				if (verify && sig.isEmpty())
				{
					System.out.println("-sig is required.");
					return;
				}

				if (certfile.isEmpty())
				{
					System.out.println("-cert is required.");
					return;
				}

				byte[] inputB = input.getBytes();

				// load key from certificate file
				Cryptokeymanager keymanager = new Cryptokeymanager();
				keymanager.setCertificate(loadCertificate(certfile, certpass));
				keymanager.importFromCert();
				crypto.setKey(keymanager.getKey());

				if (sign)
				{
					if (compact)
					{
						crypto.setOutputEncoding(cetCompact);
					}
					else
					{
						crypto.setOutputEncoding(cetJSON);
					}

					byte[] outputB = crypto.sign(inputB, true);

					System.out.println("Signature: " + new String(outputB));
				}
				else
				{
					if (compact)
					{
						crypto.setInputEncoding(cetCompact);
					}
					else
					{
						crypto.setInputEncoding(cetJSON);
					}

					byte[] sigB = sig.getBytes();

					crypto.verifyDetached(inputB, sigB);

					switch (crypto.getSignatureValidationResult())
					{
						case svtValid: System.out.println("Verification succeeded"); break;
						case svtCorrupted: System.out.println("Verification corrupted"); break;
						case svtFailure: System.out.println("Verification failed"); break;
						default: System.out.println("Verification unknown"); break;
					}
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



