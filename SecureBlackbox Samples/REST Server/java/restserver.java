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

public class restserver extends ConsoleDemo {
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
				"  restserver -- SecureBlackbox RESTServer Demo Application\n\n" +
				"SYNOPSIS\n" +
				"  restserver <listening_port> [-tlsmode tlsmode] [-cert certificate_file] [-certpass certificate_password] \n\n" +
				"DESCRIPTION\n" +
				"  RESTServer demonstrates the usage of RESTServer from SecureBlackbox.\n" +
				"  The options are as follows:\n\n" +
				"  -tlsmode        The TLS mode. Enter the corresponding string. Valid values: none, explicit, implicit.\n\n" +
				"  -cert           The certificate used in rest server.\n\n" +
				"  -certpass       The password for the certificate.\n\n" +
				"EXAMPLES\n" +
				"  restserver 80 \n\n" +
				"  restserver 8080 -tlsmode implicit -cert C:\\certs\\mycert.pfx -certpass mypassword \n"
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
		if (args.length < 1) {
			displayHelp("listening_port is required.");
			return;
		}

		RESTServer server = new RESTServer();
		try {
			server.setPort(Integer.parseInt(args[0]));
		} catch (Exception ex) {
			displayHelp("Invalid port value");
			return;
		}

		try {
			server.addRESTServerEventListener(new RESTServerEventListener() {
				@Override
				public void accept(RESTServerAcceptEvent e) {
					e.accept = true;
					System.out.println("Client connected from " + e.remoteAddress + ":" + e.remotePort);
				}

				@Override
				public void authAttempt(RESTServerAuthAttemptEvent e) {

				}

				@Override
				public void connect(RESTServerConnectEvent e) {

				}

				@Override
				public void customRequest(RESTServerCustomRequestEvent e) {

				}

				@Override
				public void data(RESTServerDataEvent e) {

				}

				@Override
				public void deleteRequest(RESTServerDeleteRequestEvent e) {
					System.out.println("Delete request from " + e.connectionID);
				}

				@Override
				public void disconnect(RESTServerDisconnectEvent e) {

				}

				@Override
				public void error(RESTServerErrorEvent e) {

				}

				@Override
				public void externalSign(RESTServerExternalSignEvent e) {

				}

				@Override
				public void fileError(RESTServerFileErrorEvent e) {

				}

				@Override
				public void getRequest(RESTServerGetRequestEvent e) {
					System.out.println("Get request from " + e.connectionID);
				}

				@Override
				public void headersPrepared(RESTServerHeadersPreparedEvent e) {

				}

				@Override
				public void headRequest(RESTServerHeadRequestEvent e) {

				}

				@Override
				public void notification(RESTServerNotificationEvent e) {

				}

				@Override
				public void optionsRequest(RESTServerOptionsRequestEvent e) {

				}

				@Override
				public void patchRequest(RESTServerPatchRequestEvent e) {

				}

				@Override
				public void postRequest(RESTServerPostRequestEvent e) {
					System.out.println("Post request from " + e.connectionID);
				}

				@Override
				public void putRequest(RESTServerPutRequestEvent e) {

				}

				@Override
				public void resourceAccess(RESTServerResourceAccessEvent e) {

				}

				@Override
				public void TLSCertValidate(RESTServerTLSCertValidateEvent e) {

				}

				@Override
				public void TLSEstablished(RESTServerTLSEstablishedEvent e) {

				}

				@Override
				public void TLSHandshake(RESTServerTLSHandshakeEvent e) {

				}

				@Override
				public void TLSPSK(RESTServerTLSPSKEvent e) {

				}

				@Override
				public void TLSShutdown(RESTServerTLSShutdownEvent e) {

				}

				@Override
				public void traceRequest(RESTServerTraceRequestEvent e) {

				}

				@Override
				public void supercoreIntercept(RESTServerSupercoreInterceptEvent e) {

				}
			});

			server.getTLSSettings().setTLSMode(TLSSettings.smNoTLS);
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

			if (optext(args, "-cert")) {
				CertificateManager cm = new CertificateManager();
				cm.importFromFile(optval(args, "-cert"), optval(args, "-certpass"));
				server.getTLSServerChain().add(cm.getCertificate());
			} else {
				if (server.getTLSSettings().getTLSMode() != TLSSettings.smNoTLS) {
					displayHelp("The server cannot support TLS without a valid server certificate. Please provide a certificate file via the cert and certpass parameters.");
					return;
				}
			}

			server.start();

			System.out.println("REST server started on port " + server.getPort() + ". Press enter to stop server.");
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



