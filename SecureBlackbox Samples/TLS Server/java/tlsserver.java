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
import java.util.Arrays;

import secureblackbox.*;

public class tlsserver extends ConsoleDemo {
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
				"  tlsserver -- SecureBlackbox TLSServer Demo Application\n\n" +
				"SYNOPSIS\n" +
				"  tlsserver <listening_port> [-cert certificate_file] [-certpass certificate_password] [-tlsmode tlsmode]\n\n" +
				"DESCRIPTION\n" +
				"  TLSServer demonstrates the usage of TLSServer from SecureBlackbox.\n" +
				"  The options are as follows:\n\n" +
				"  -cert           The certificate used in tls server.\n\n" +
				"  -certpass       The password for the certificate.\n\n" +
				"  -tlsmode        The TLS mode. Enter the corresponding string. Valid values: none, implicit.\n\n" +
				"EXAMPLES\n" +
				"  tlsserver 80 \n\n" +
				"  tlsserver 8080 -cert C:\\certs\\mycert.pfx -certpass mypassword -tlsmode implicit \n"
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

	private static TLSServer server;

	public static void main(String[] args) {
		if (args.length < 1) {
			displayHelp("listening_port is required.");
			return;
		}

		server = new TLSServer();
		try {
			server.setPort(Integer.parseInt(args[0]));
		} catch (Exception ex) {
			System.out.println();
			displayHelp("Invalid port value");
			return;
		}

		try {
			server.addTLSServerEventListener(new TLSServerEventListener() {
				@Override
				public void accept(TLSServerAcceptEvent e) {
					e.accept = true;
					System.out.println("Accepted a new client from " + e.remoteAddress + ":" + e.remotePort);
				}

				@Override
				public void connect(TLSServerConnectEvent e) {

				}

				@Override
				public void data(TLSServerDataEvent e) {
					System.out.println(" [" + e.connectionID + "] C->S: " + new String(e.buffer));

					byte[] dst = Arrays.copyOf(e.buffer, e.buffer.length);
					for (int i = 0, j = e.buffer.length - 1; i < j; i++, j--) {
						dst[i] = e.buffer[j];
						dst[j] = e.buffer[i];
					}
					try {
						server.sendData(e.connectionID, dst);
						System.out.println(" [" + e.connectionID + "] S->C: " + new String(dst));
					} catch (Exception ex) {
						ConsoleDemo.displayError(ex);
					}
				}

				@Override
				public void disconnect(TLSServerDisconnectEvent e) {

				}

				@Override
				public void error(TLSServerErrorEvent e) {
					System.out.println(" [" + e.connectionID + "] Error " + e.errorCode + ": " + e.description);
				}

				@Override
				public void externalSign(TLSServerExternalSignEvent e) {

				}

				@Override
				public void notification(TLSServerNotificationEvent e) {

				}

				@Override
				public void TLSCertValidate(TLSServerTLSCertValidateEvent e) {

				}

				@Override
				public void TLSEstablished(TLSServerTLSEstablishedEvent e) {
					System.out.println(" [" + e.connectionID + "] Secure session established");
				}

				@Override
				public void TLSHandshake(TLSServerTLSHandshakeEvent e) {

				}

				@Override
				public void TLSPSK(TLSServerTLSPSKEvent e) {

				}

				@Override
				public void TLSShutdown(TLSServerTLSShutdownEvent e) {
					System.out.println(" [" + e.connectionID + "] Secure session closed");
				}

				@Override
				public void supercoreIntercept(TLSServerSupercoreInterceptEvent e) {

				}
			});

			if (optext(args, "-cert")) {
				CertificateManager cm = new CertificateManager();
				cm.importFromFile(optval(args, "-cert"), optval(args, "-certpass"));
				server.getTLSServerChain().add(cm.getCertificate());
			}

			if (optext(args, "-tlsmode")) {
				String tlsmode = optval(args, "-tlsmode");
				if (tlsmode.equalsIgnoreCase("none")) {
					server.getTLSSettings().setTLSMode(TLSSettings.smNoTLS);
				} else if (tlsmode.equalsIgnoreCase("explicit")) {
					server.getTLSSettings().setTLSMode(TLSSettings.smExplicitTLS);
				} else if (tlsmode.equalsIgnoreCase("implicit")) {
					server.getTLSSettings().setTLSMode(TLSSettings.smImplicitTLS);
				}
			}

			server.start();

			System.out.println("TLS server started on port " + server.getPort() + ". Press enter to stop server.");
			input();

			server.stop();

			System.out.println("Server stopped.\n");

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



