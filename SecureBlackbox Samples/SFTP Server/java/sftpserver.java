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

public class sftpserver extends ConsoleDemo {
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
				"  sftpserver -- SecureBlackbox SFTPServer Demo Application\n\n" +
				"SYNOPSIS\n" +
				"  sftpserver <listening_port> [-users users_file] [-userspass users_password] [-key key_file] [-keypass key_password]\n" +
				"             [-basedir base_directory]\n\n" +
				"DESCRIPTION\n" +
				"  SFTPServer demonstrates the usage of SFTPServer from SecureBlackbox.\n" +
				"  The options are as follows:\n\n" +
				"  -users          A file containing user login parameters.\n\n" +
				"  -userspass      The password for the users file.\n\n" +
				"  -key            A file containing the private host key.\n\n" +
				"  -keypass        The password for the key file.\n\n" +
				"  -basedir        The base directory of the server.\n\n" +
				"EXAMPLES\n" +
				"  sftpserver 22 \n\n" +
				"  sftpserver 2222 -users C:\\sftpserver\\users.dat -key C:\\certs\\mykey.pem -keypass mypassword -basedir D:\\temp \n"
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

		SFTPServer server = new SFTPServer();
		try {
			server.setPort(Integer.parseInt(args[0]));
		} catch (Exception ex) {
			displayHelp("Invalid port value");
			return;
		}

		try {
			server.addSFTPServerEventListener(new SFTPServerEventListener() {
				@Override
				public void accept(SFTPServerAcceptEvent e) {

				}

				@Override
				public void afterCreateDirectory(SFTPServerAfterCreateDirectoryEvent e) {

				}

				@Override
				public void afterRemove(SFTPServerAfterRemoveEvent e) {

				}

				@Override
				public void afterRenameFile(SFTPServerAfterRenameFileEvent e) {

				}

				@Override
				public void afterRequestAttributes(SFTPServerAfterRequestAttributesEvent e) {

				}

				@Override
				public void afterSetAttributes(SFTPServerAfterSetAttributesEvent e) {

				}

				@Override
				public void authAttempt(SFTPServerAuthAttemptEvent e) {
					if (e.accept) {
						System.out.println("Access granted to user " + e.username);
					} else {
						System.out.println("Access denied for user " + e.username);
					}
				}

				@Override
				public void authFailed(SFTPServerAuthFailedEvent e) {

				}

				@Override
				public void authPassword(SFTPServerAuthPasswordEvent e) {

				}

				@Override
				public void authPublicKey(SFTPServerAuthPublicKeyEvent e) {

				}

				@Override
				public void authSucceeded(SFTPServerAuthSucceededEvent e) {

				}

				@Override
				public void beforeCreateDirectory(SFTPServerBeforeCreateDirectoryEvent e) {

				}

				@Override
				public void beforeDownloadFile(SFTPServerBeforeDownloadFileEvent e) {

				}

				@Override
				public void beforeFind(SFTPServerBeforeFindEvent e) {

				}

				@Override
				public void beforeOpenClientForwarding(SFTPServerBeforeOpenClientForwardingEvent e) {

				}

				@Override
				public void beforeOpenCommand(SFTPServerBeforeOpenCommandEvent e) {

				}

				@Override
				public void beforeOpenServerForwarding(SFTPServerBeforeOpenServerForwardingEvent e) {

				}

				@Override
				public void beforeOpenShell(SFTPServerBeforeOpenShellEvent e) {

				}

				@Override
				public void beforeRemove(SFTPServerBeforeRemoveEvent e) {

				}

				@Override
				public void beforeRenameFile(SFTPServerBeforeRenameFileEvent e) {

				}

				@Override
				public void beforeRequestAttributes(SFTPServerBeforeRequestAttributesEvent e) {

				}

				@Override
				public void beforeSetAttributes(SFTPServerBeforeSetAttributesEvent e) {

				}

				@Override
				public void beforeUploadFile(SFTPServerBeforeUploadFileEvent e) {

				}

				@Override
				public void closeClientForwarding(SFTPServerCloseClientForwardingEvent e) {

				}

				@Override
				public void closeCommand(SFTPServerCloseCommandEvent e) {

				}

				@Override
				public void closeFile(SFTPServerCloseFileEvent e) {

				}

				@Override
				public void closeServerForwarding(SFTPServerCloseServerForwardingEvent e) {

				}

				@Override
				public void closeShell(SFTPServerCloseShellEvent e) {

				}

				@Override
				public void connect(SFTPServerConnectEvent e) {
					System.out.println("Client connected from " + e.remoteAddress + ":" + e.remotePort);
				}

				@Override
				public void createDirectory(SFTPServerCreateDirectoryEvent e) {

				}

				@Override
				public void disconnect(SFTPServerDisconnectEvent e) {

				}

				@Override
				public void error(SFTPServerErrorEvent e) {

				}

				@Override
				public void externalSign(SFTPServerExternalSignEvent e) {

				}

				@Override
				public void findClose(SFTPServerFindCloseEvent e) {

				}

				@Override
				public void findFirst(SFTPServerFindFirstEvent e) {

				}

				@Override
				public void findNext(SFTPServerFindNextEvent e) {

				}

				@Override
				public void listeningStarted(SFTPServerListeningStartedEvent e) {

				}

				@Override
				public void listeningStopped(SFTPServerListeningStoppedEvent e) {

				}

				@Override
				public void notification(SFTPServerNotificationEvent e) {

				}

				@Override
				public void openClientForwarding(SFTPServerOpenClientForwardingEvent e) {

				}

				@Override
				public void openCommand(SFTPServerOpenCommandEvent e) {

				}

				@Override
				public void openFile(SFTPServerOpenFileEvent e) {

				}

				@Override
				public void openServerForwarding(SFTPServerOpenServerForwardingEvent e) {

				}

				@Override
				public void openShell(SFTPServerOpenShellEvent e) {

				}

				@Override
				public void readFile(SFTPServerReadFileEvent e) {

				}

				@Override
				public void remove(SFTPServerRemoveEvent e) {

				}

				@Override
				public void renameFile(SFTPServerRenameFileEvent e) {

				}

				@Override
				public void requestAttributes(SFTPServerRequestAttributesEvent e) {

				}

				@Override
				public void serverForwardingCancel(SFTPServerServerForwardingCancelEvent e) {

				}

				@Override
				public void serverForwardingOpenFailed(SFTPServerServerForwardingOpenFailedEvent e) {

				}

				@Override
				public void serverForwardingRequest(SFTPServerServerForwardingRequestEvent e) {

				}

				@Override
				public void sessionClosed(SFTPServerSessionClosedEvent e) {

				}

				@Override
				public void sessionEstablished(SFTPServerSessionEstablishedEvent e) {

				}

				@Override
				public void setAttributes(SFTPServerSetAttributesEvent e) {

				}

				@Override
				public void translatePath(SFTPServerTranslatePathEvent e) {

				}

				@Override
				public void writeFile(SFTPServerWriteFileEvent e) {

				}

				@Override
				public void supercoreIntercept(SFTPServerSupercoreInterceptEvent e) {

				}
			});

			if (optext(args, "-basedir")) {
				server.setBaseDir(optval(args, "-basedir"));
			}

			if (optext(args, "-key")) {
				SSHKeyManager km = new SSHKeyManager();
				km.importFromFile(optval(args, "-key"), optval(args, "-keypass"));
				server.getServerKeys().add(km.getKey());
			}

			if (optext(args, "-users")) {
				UserManager um = new UserManager();
				um.importFromFile(optval(args, "-users"), optval(args, "-userspass"), true);
				server.setUsers(um.getUsers());
			}

			server.start();

			System.out.println("SFTP server started on port " + server.getPort() + ". Press enter to stop server.");
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



