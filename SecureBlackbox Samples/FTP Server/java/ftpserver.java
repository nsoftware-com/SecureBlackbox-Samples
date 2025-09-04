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

public class ftpserver extends ConsoleDemo {
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
				"  ftpserver -- SecureBlackbox FTPServer Demo Application\n\n" +
				"SYNOPSIS\n" +
				"  ftpserver <listening_port> [-users users_file] [-userspass users_password] [-cert certificate_file]\n" +
				"              [-certpass certificate_password] [-tlsmode tlsmode] [-allowanon]\n\n" +
				"DESCRIPTION\n" +
				"  FTPServer demonstrates the usage of FTPServer from SecureBlackbox.\n" +
				"  The options are as follows:\n\n" +
				"  -users          An file with users information.\n\n" +
				"  -userspass      The password for users file.\n\n" +
				"  -cert           The certificate used in ftp server.\n\n" +
				"  -certpass       The password for the certificate.\n\n" +
				"  -tlsmode        The TLS mode. Enter the corresponding string. Valid values: none, explicit, implicit.\n\n" +
				"  -allowanon      Whether to allow connection from anonymous user.\n\n" +
				"EXAMPLES\n" +
				"  ftpserver 80 \n\n" +
				"  ftpserver 8080 -users C:\\ftpserver\\users.dat -cert C:\\certs\\mycert.pfx -certpass mypassword \n" +
				"             -tlsmode implicit -allowanon \n"
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

		FTPServer server = new FTPServer();
		try {
			server.setPort(Integer.parseInt(args[0]));
		} catch (Exception ex) {
			displayHelp("Invalid port value");
			return;
		}

		try {
			server.addFTPServerEventListener(new FTPServerEventListener() {
				@Override
				public void accept(FTPServerAcceptEvent e) {

				}

				@Override
				public void afterChangeDirectory(FTPServerAfterChangeDirectoryEvent e) {

				}

				@Override
				public void afterCreateDirectory(FTPServerAfterCreateDirectoryEvent e) {

				}

				@Override
				public void afterRemoveDirectory(FTPServerAfterRemoveDirectoryEvent e) {

				}

				@Override
				public void afterRemoveFile(FTPServerAfterRemoveFileEvent e) {

				}

				@Override
				public void afterRenameFile(FTPServerAfterRenameFileEvent e) {

				}

				@Override
				public void afterRequestAttributes(FTPServerAfterRequestAttributesEvent e) {

				}

				@Override
				public void authAttempt(FTPServerAuthAttemptEvent e) {
					if (e.allow) {
						System.out.println("Access granted to user " + e.username);
					} else {
						System.out.println("Access denied for user " + e.username);
					}
				}

				@Override
				public void beforeChangeDirectory(FTPServerBeforeChangeDirectoryEvent e) {

				}

				@Override
				public void beforeCreateDirectory(FTPServerBeforeCreateDirectoryEvent e) {

				}

				@Override
				public void beforeDownloadFile(FTPServerBeforeDownloadFileEvent e) {

				}

				@Override
				public void beforeFind(FTPServerBeforeFindEvent e) {

				}

				@Override
				public void beforeRemoveDirectory(FTPServerBeforeRemoveDirectoryEvent e) {

				}

				@Override
				public void beforeRemoveFile(FTPServerBeforeRemoveFileEvent e) {

				}

				@Override
				public void beforeRenameFile(FTPServerBeforeRenameFileEvent e) {

				}

				@Override
				public void beforeRequestAttributes(FTPServerBeforeRequestAttributesEvent e) {

				}

				@Override
				public void beforeSendReply(FTPServerBeforeSendReplyEvent e) {

				}

				@Override
				public void beforeUploadFile(FTPServerBeforeUploadFileEvent e) {

				}

				@Override
				public void changeDirectory(FTPServerChangeDirectoryEvent e) {

				}

				@Override
				public void commandProcessed(FTPServerCommandProcessedEvent e) {

				}

				@Override
				public void commandReceived(FTPServerCommandReceivedEvent e) {

				}

				@Override
				public void connect(FTPServerConnectEvent e) {
					System.out.println("Client connected from " + e.remoteAddress + ":" + e.port);
				}

				@Override
				public void createDirectory(FTPServerCreateDirectoryEvent e) {

				}

				@Override
				public void disconnect(FTPServerDisconnectEvent e) {

				}

				@Override
				public void downloadBegin(FTPServerDownloadBeginEvent e) {

				}

				@Override
				public void downloadEnd(FTPServerDownloadEndEvent e) {

				}

				@Override
				public void downloadFile(FTPServerDownloadFileEvent e) {

				}

				@Override
				public void downloadProgress(FTPServerDownloadProgressEvent e) {

				}

				@Override
				public void error(FTPServerErrorEvent e) {

				}

				@Override
				public void externalSign(FTPServerExternalSignEvent e) {

				}

				@Override
				public void findClose(FTPServerFindCloseEvent e) {

				}

				@Override
				public void findInit(FTPServerFindInitEvent e) {

				}

				@Override
				public void findNext(FTPServerFindNextEvent e) {

				}

				@Override
				public void notification(FTPServerNotificationEvent e) {

				}

				@Override
				public void readFile(FTPServerReadFileEvent e) {

				}

				@Override
				public void removeDirectory(FTPServerRemoveDirectoryEvent e) {

				}

				@Override
				public void removeFile(FTPServerRemoveFileEvent e) {

				}

				@Override
				public void renameFile(FTPServerRenameFileEvent e) {

				}

				@Override
				public void requestAttributes(FTPServerRequestAttributesEvent e) {

				}

				@Override
				public void TLSCertValidate(FTPServerTLSCertValidateEvent e) {

				}

				@Override
				public void TLSEstablished(FTPServerTLSEstablishedEvent e) {

				}

				@Override
				public void TLSHandshake(FTPServerTLSHandshakeEvent e) {

				}

				@Override
				public void TLSPSK(FTPServerTLSPSKEvent e) {

				}

				@Override
				public void TLSShutdown(FTPServerTLSShutdownEvent e) {

				}

				@Override
				public void transferCompleted(FTPServerTransferCompletedEvent e) {

				}

				@Override
				public void uploadBegin(FTPServerUploadBeginEvent e) {

				}

				@Override
				public void uploadEnd(FTPServerUploadEndEvent e) {

				}

				@Override
				public void uploadFile(FTPServerUploadFileEvent e) {

				}

				@Override
				public void uploadProgress(FTPServerUploadProgressEvent e) {

				}

				@Override
				public void writeFile(FTPServerWriteFileEvent e) {

				}

				@Override
				public void supercoreIntercept(FTPServerSupercoreInterceptEvent e) {

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

			if (optext(args, "-users")) {
				UserManager um = new UserManager();
				um.importFromFile(optval(args, "-users"), optval(args, "-userspass"), true);
				server.setUsers(um.getUsers());
			}

			if (optext(args, "-allowanon")) {
				server.setAllowAnonymous(true);
			}

			server.start();

			System.out.println("FTP server started on port " + server.getPort() + ". Press enter to stop server.");
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



