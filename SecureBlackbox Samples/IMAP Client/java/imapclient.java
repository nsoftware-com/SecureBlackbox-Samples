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
import java.util.ArrayList;
import java.util.StringTokenizer;

import secureblackbox.*;

public class imapclient extends ConsoleDemo {
	public static String[] translateCommandline(String toProcess) {
		if (toProcess == null || toProcess.length() == 0) {
			return new String[0];
		}

		final int normal = 0;
		final int inQuote = 1;
		final int inDoubleQuote = 2;
		int state = normal;
		final StringTokenizer tok = new StringTokenizer(toProcess, "\"\' ", true);
		final ArrayList<String> result = new ArrayList<String>();
		final StringBuilder current = new StringBuilder();
		boolean lastTokenHasBeenQuoted = false;

		while (tok.hasMoreTokens()) {
			String nextTok = tok.nextToken();
			switch (state) {
				case inQuote:
					if ("\'".equals(nextTok)) {
						lastTokenHasBeenQuoted = true;
						state = normal;
					} else {
						current.append(nextTok);
					}
					break;
				case inDoubleQuote:
					if ("\"".equals(nextTok)) {
						lastTokenHasBeenQuoted = true;
						state = normal;
					} else {
						current.append(nextTok);
					}
					break;
				default:
					if ("\'".equals(nextTok)) {
						state = inQuote;
					} else if ("\"".equals(nextTok)) {
						state = inDoubleQuote;
					} else if (" ".equals(nextTok)) {
						if (lastTokenHasBeenQuoted || current.length() != 0) {
							result.add(current.toString());
							current.setLength(0);
						}
					} else {
						current.append(nextTok);
					}
					lastTokenHasBeenQuoted = false;
					break;
			}
		}
		if (lastTokenHasBeenQuoted || current.length() != 0) {
			result.add(current.toString());
		}
		if (state == inQuote || state == inDoubleQuote) {
			throw new RuntimeException("unbalanced quotes in " + toProcess);
		}
		return result.toArray(new String[result.size()]);
	}

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
				"  imapclient -- SecureBlackbox IMAPClient Demo Application\n\n" +
				"SYNOPSIS\n" +
				"  imapclient <host> <port> [-username username] [-password password] [-tlsmode tlsmode]\n\n" +
				"DESCRIPTION\n" +
				"  This sample how to deal with IMAP4 servers. It can list mailboxes on the server, \n" +
				"  list messages in the selected mailbox, download them from the server and upload local messages to the server.\n\n" +
				"  The options are as follows:\n\n" +
				"  host        The name or address of a mail server (Required).\n\n" +
				"  port        The port of a mail server (Required).\n\n" +
				"  -username   The user identifier for the mailbox.\n\n" +
				"  -password   The password for the mailbox user.\n\n" +
				"  -tlsmode    The TLS mode. Enter the corresponding string. Valid values: none, explicit, implicit.\n\n" +
				"EXAMPLES\n" +
				"  imapclient mail.local 995\n\n" +
				"  imapclient mail.local 12345 -username testuser -password pass -tlsmode implicit\n"
		);

		if (errMes.length() > 0) {
			System.out.println("Error: " + errMes);
			System.out.println();
		}

		confirmExit();
	}

	private static void displayCommands() {
		System.out.println("IMAP Commands\n" +
				"l                               list all mailboxes\n" +
				"s <mailbox id>                  select mailbox\n" +
				"e <mailbox id>                  examine mailbox\n" +
				"c <mailbox>                     create mailbox\n" +
				"d <mailbox id>                  delete mailbox\n" +
				"n                               goto and type next message\n" +
				"p                               goto and type previous message\n" +
				"pnum                            print out active message number\n" +
				"h                               print out active message headers\n" +
				"fa                              give head lines of messages(list all messages)\n" +
				"fd                              give head lines of deleted messages(list deleted messages)\n" +
				"fn                              give head lines of new messages(list new messages)\n" +
				"fr                              give head lines of recent messages(list recent messages)\n" +
				"fu                              give head lines of unseen messages(list unseen messages)\n" +
				"md                              mark message deleted\n" +
				"pu                              purge messages\n" +
				"r <filename>                    receive file\n" +
				"po <filename>                   post file\n" +
				"q                               quit, saving unresolved messages in mbox\n" +
				"<message number>                switch to new active message");
	}

	private static void loadMessages(IMAPClient client) {
		System.out.println("Messages:");
		for (int i = 0; i < client.getMessages().size(); i++) {
			System.out.println("  #" + i);
			System.out.println("    From:    " + client.getMessages().item(i).getFrom());
			System.out.println("    To:      " + client.getMessages().item(i).getSentTo());
			System.out.println("    Subject: " + client.getMessages().item(i).getSubject());
			System.out.println("    Date:    " + client.getMessages().item(i).getDate());
			System.out.println("    Size:    " + client.getMessages().item(i).getSize());
		}
	}

	private static void loadCurMailbox(IMAPClient client) {
		System.out.println("Name: " + client.getCurrentMailbox().getName());
		System.out.println("Total: " + client.getCurrentMailbox().getTotalMessages());
		System.out.println("Unseen: " + client.getCurrentMailbox().getUnseenMessages());
		System.out.println("Recent: " + client.getCurrentMailbox().getRecentMessages());
		System.out.println("Read-only: " + (client.getCurrentMailbox().getReadOnly() ? "Yes" : "No"));
	}

	private static void loadMailboxes(IMAPClient client) {
		System.out.println("Mailboxes:");
		for (int i = 0; i < client.getMailboxes().size(); i++) {
			System.out.println("  #" + i);
			System.out.println("    Name:    " + client.getMailboxes().item(i).getName());
			System.out.println("    Children:      " + (client.getMailboxes().item(i).getHasChildren() ? "Yes" : "No"));
			System.out.println("    Marked: " + (client.getMailboxes().item(i).getUnmarked() ? "Yes" : "No"));
			System.out.println("    Select:    " + (client.getMailboxes().item(i).getNoSelect() ? "Yes" : "No"));
			System.out.println("    Inferiors:    " + (client.getMailboxes().item(i).getNoInferiors() ? "Yes" : "No"));
		}
	}

	private static boolean isInteger(String s) {
		return isInteger(s,10);
	}

	private static boolean isInteger(String s, int radix) {
		if(s.isEmpty()) return false;
		for(int i = 0; i < s.length(); i++) {
			if(i == 0 && s.charAt(i) == '-') {
				if(s.length() == 1) return false;
				else continue;
			}
			if(Character.digit(s.charAt(i),radix) < 0) return false;
		}
		return true;
	}

	private static void confirmExit() {
		System.out.println("Press Enter to exit the demo.");
		input();
	}

	public static void main(String[] args) {
		if (args.length < 2) {
			displayHelp("host and port is required.");
			return;
		}

		IMAPClient client = new IMAPClient();
		int msgnum = 0; // current message number for next command

		try {
			if (optext(args, "-username")) {
				client.setUsername(optval(args, "-username"));
			}

			if (optext(args, "-password")) {
				client.setPassword(optval(args, "-password"));
			}

			if (optext(args, "-tlsmode")) {
				String tlsmode = optval(args, "-tlsmode");
				if (tlsmode.equalsIgnoreCase("none")) {
					client.getTLSSettings().setTLSMode(TLSSettings.smNoTLS);
				} else if (tlsmode.equalsIgnoreCase("explicit")) {
					client.getTLSSettings().setTLSMode(TLSSettings.smExplicitTLS);
				} else if (tlsmode.equalsIgnoreCase("implicit")) {
					client.getTLSSettings().setTLSMode(TLSSettings.smImplicitTLS);
				}
			}

			client.getTLSSettings().setRevocationCheck(TLSSettings.crcAllCRL);

			client.connect(args[0], Integer.parseInt(args[1]));

			System.out.println("Connected successfully.");

			// main loop to check for commands
			while (true) {
				String[] commands = translateCommandline(prompt("imap", ">"));

				if (commands.length > 0) {
					if (commands[0].equalsIgnoreCase("s")) {
						if (commands.length < 2) {
							System.out.println("You need to pass <mailbox id> parameter");
						} else {
							client.selectMailbox(client.getMailboxes().item(Integer.parseInt(commands[1])).getName());

							loadCurMailbox(client);
						}
					} else if (commands[0].equalsIgnoreCase("e")) {
						if (commands.length < 2) {
							System.out.println("You need to pass <mailbox id> parameter");
						} else {
							client.examineMailbox(client.getMailboxes().item(Integer.parseInt(commands[1])).getName());

							loadCurMailbox(client);
						}
					} else if (commands[0].equalsIgnoreCase("c")) {
						if (commands.length < 2) {
							System.out.println("You need to pass <mailbox> parameter");
						} else {
							client.createMailbox(commands[1]);

							loadCurMailbox(client);
						}
					} else if (commands[0].equalsIgnoreCase("d")) {
						if (commands.length < 2) {
							System.out.println("You need to pass <mailbox id> parameter");
						} else {
							client.deleteMailbox(client.getMailboxes().item(Integer.parseInt(commands[1])).getName());

							loadCurMailbox(client);
						}
					} else if (commands[0].equalsIgnoreCase("fa")) {
						client.listAllMessages();

						loadMessages(client);
					} else if (commands[0].equalsIgnoreCase("fd")) {
						client.listDeletedMessages();

						loadMessages(client);
					} else if (commands[0].equalsIgnoreCase("fn")) {
						client.listNewMessages();

						loadMessages(client);
					} else if (commands[0].equalsIgnoreCase("fr")) {
						client.listRecentMessages();

						loadMessages(client);
					} else if (commands[0].equalsIgnoreCase("fu")) {
						client.listUnseenMessages();

						loadMessages(client);
					} else if (commands[0].equalsIgnoreCase("md")) {
						client.markMessageDeleted(client.getMessages().item(msgnum).getUID());

						loadCurMailbox(client);
					} else if (commands[0].equalsIgnoreCase("pu")) {
						client.purgeMessages();

						loadCurMailbox(client);
						loadMessages(client);
					} else if (commands[0].equalsIgnoreCase("r")) {
						if (commands.length < 2) {
							System.out.println("You need to pass <filename> parameter");
						} else {
							client.receiveFile(client.getMessages().item(msgnum).getUID(), commands[1]);

							System.out.println("The message has been received successfully.");
						}
					} else if (commands[0].equalsIgnoreCase("po")) {
						if (commands.length < 2) {
							System.out.println("You need to pass <filename> parameter");
						} else {
							client.postFile(commands[1], 0, "");

							loadMessages(client);
							System.out.println("The message has been posted successfully.");
						}
					} else if (commands[0].equalsIgnoreCase("h")) {
						if (client.getMessages().size() > msgnum) {
							System.out.println("From:    " + client.getMessages().item(msgnum).getFrom());
							System.out.println("To:      " + client.getMessages().item(msgnum).getSentTo());
							System.out.println("Subject: " + client.getMessages().item(msgnum).getSubject());
							System.out.println("Date:    " + client.getMessages().item(msgnum).getDate());
							System.out.println("Size:    " + client.getMessages().item(msgnum).getSize());
						}
						else {
							System.out.println("No info with active message number");
						}
					} else if (commands[0].equalsIgnoreCase("l")) {
						client.listMailboxes();

						loadMailboxes(client);
					} else if (commands[0].equalsIgnoreCase("n")) {
						if (msgnum < client.getMessages().size() - 1) {
							msgnum++;
						}
						else {
							System.out.println("The active letter cannot be assigned to switch to the next letter, since the current letter is the last one.");
						}
					} else if (commands[0].equalsIgnoreCase("p")) {
						if (msgnum > 0) {
							msgnum--;
						}
						else {
							System.out.println("The active letter cannot be assigned to switch to the previous letter, since the current letter is the first.");
						}
					} else if (commands[0].equalsIgnoreCase("pnum")) {
						System.out.println(msgnum);
					} else if (commands[0].equalsIgnoreCase("bye") || commands[0].equalsIgnoreCase("q") || commands[0].equalsIgnoreCase("quit")) {
						client.disconnect();
						break;
					} else if (commands[0].equalsIgnoreCase("?") || commands[0].equalsIgnoreCase("help") || commands[0].equalsIgnoreCase("man")) {
						displayCommands();
					} else {
                        if (isInteger(commands[0])) {
                            int newNumb = Integer.parseInt(commands[0]);
                            if (newNumb >= 0 && newNumb < client.getMessages().size()) {
                                msgnum = newNumb;
                            }
                            else {
                                System.out.println("Not valid number.");
                            }
                        } else {
                            System.out.println("Command not recognized. Choose from these:\n");
                            displayCommands();
                        }
					}
				} else {
					System.out.println("Command not recognized. Choose from these:\n");
					displayCommands();
				}
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



