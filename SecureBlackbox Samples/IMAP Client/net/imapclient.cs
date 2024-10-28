/*
 * SecureBlackbox 2024 .NET Edition - Sample Project
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
 * 
 */

﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using nsoftware.SecureBlackbox;

class imapclient
{
    private static string optval(string[] args, string option)
    {
        for (int x = 0; x < args.Length - 1; x++)
        {
            if (args[x].Equals(option, StringComparison.CurrentCultureIgnoreCase))
            {
                return args[x + 1];
            }
        }
        return "";
    }

    private static bool optext(string[] args, string option)
    {
        for (int x = 0; x < args.Length; x++)
        {
            if (args[x].Equals(option, StringComparison.CurrentCultureIgnoreCase))
            {
                return true;
            }
        }
        return false;
    }

    private static void displayHelp(string errMes)
    {
        Console.WriteLine(
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

        if (errMes.Length > 0)
        {
            Console.WriteLine("Error: " + errMes);
            Console.WriteLine();
        }

        confirmExit();
    }

    private static List<string> waitCommand(string promptMes)
    {
        Console.Write(promptMes);
        List<string> result = Regex.Matches(Console.ReadLine(), @"[\""].+?[\""]|[^ ]+").Cast<Match>().Select(m => m.Value).ToList();
        for (int x = 0; x < result.Count; x++)
        {
            if (result[x].StartsWith("\"") && result[x].EndsWith("\""))
            {
                result[x] = result[x].Remove(result[x].Length - 1);
                result[x] = result[x].Remove(0, 1);
            }
        }

        return result;
    }

    private static void displayCommands()
    {
        Console.WriteLine("IMAP Commands\n" +
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

    private static void loadMessages(IMAPClient client)
    {
        Console.WriteLine("Messages:");
        for (int i = 0; i < client.Messages.Count; i++)
        {
            Console.WriteLine("  #" + i);
            Console.WriteLine("    From:    " + client.Messages[i].From);
            Console.WriteLine("    To:      " + client.Messages[i].SentTo);
            Console.WriteLine("    Subject: " + client.Messages[i].Subject);
            Console.WriteLine("    Date:    " + client.Messages[i].Date);
            Console.WriteLine("    Size:    " + client.Messages[i].Size);
        }
    }

    private static void loadCurMailbox(IMAPClient client)
    {
        Console.WriteLine("Name: " + client.CurrentMailbox.Name);
        Console.WriteLine("Total: " + client.CurrentMailbox.TotalMessages);
        Console.WriteLine("Unseen: " + client.CurrentMailbox.UnseenMessages);
        Console.WriteLine("Recent: " + client.CurrentMailbox.RecentMessages);
        Console.WriteLine("Read-only: " + (client.CurrentMailbox.ReadOnly ? "Yes" : "No"));
    }

    private static void loadMailboxes(IMAPClient client)
    {
        Console.WriteLine("Mailboxes:");
        for (int i = 0; i < client.Mailboxes.Count; i++)
        {
            Console.WriteLine("  #" + i);
            Console.WriteLine("    Name:    " + client.Mailboxes[i].Name);
            Console.WriteLine("    Children:      " + (client.Mailboxes[i].HasChildren ? "Yes" : "No"));
            Console.WriteLine("    Marked: " + (client.Mailboxes[i].Unmarked ? "Yes" : "No"));
            Console.WriteLine("    Select:    " + (client.Mailboxes[i].NoSelect ? "Yes" : "No"));
            Console.WriteLine("    Inferiors:    " + (client.Mailboxes[i].NoInferiors ? "Yes" : "No"));
        }
    }
    private static void confirmExit()
    {
        Console.WriteLine("Press Enter to exit the demo.");
        Console.ReadLine();
    }

    static void Main(string[] args)
    {
        if (args.Length < 2)
        {
            displayHelp("host and port is required.");
            return;
        }

        IMAPClient client = new IMAPClient();
        int msgnum = 0; // current message number for next command

        try
        {
            if (optext(args, "-username"))
            {
                client.Username = optval(args, "-username");
            }

            if (optext(args, "-password"))
            {
                client.Password = optval(args, "-password");
            }

            if (optext(args, "-tlsmode"))
            {
                String tlsmode = optval(args, "-tlsmode");
                if (tlsmode.Equals("none", StringComparison.CurrentCultureIgnoreCase))
                {
                    client.TLSSettings.TLSMode = SSLModes.smNoTLS;
                }
                else if (tlsmode.Equals("explicit", StringComparison.CurrentCultureIgnoreCase))
                {
                    client.TLSSettings.TLSMode = SSLModes.smExplicitTLS;
                }
                else if (tlsmode.Equals("implicit", StringComparison.CurrentCultureIgnoreCase))
                {
                    client.TLSSettings.TLSMode = SSLModes.smImplicitTLS;
                }
            }

            client.TLSSettings.RevocationCheck = RevocationCheckKinds.crcAllCRL;

            client.Connect(args[0], int.Parse(args[1]));

            Console.WriteLine("Connected successfully.");

            // main loop to check for commands
            while (true)
            {
                var commands = waitCommand("imap>");

                if (commands.Count > 0)
                {
                    if (commands[0].Equals("s", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 2)
                        {
                            Console.WriteLine("You need to pass <mailbox id> parameter");
                        }
                        else
                        {
                            client.SelectMailbox(client.Mailboxes[int.Parse(commands[1])].Name);

                            loadCurMailbox(client);
                        }
                    }
                    else if (commands[0].Equals("e", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 2)
                        {
                            Console.WriteLine("You need to pass <mailbox id> parameter");
                        }
                        else
                        {
                            client.ExamineMailbox(client.Mailboxes[int.Parse(commands[1])].Name);

                            loadCurMailbox(client);
                        }
                    }
                    else if (commands[0].Equals("c", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 2)
                        {
                            Console.WriteLine("You need to pass <mailbox> parameter");
                        }
                        else
                        {
                            client.CreateMailbox(commands[1]);

                            loadCurMailbox(client);
                        }
                    }
                    else if (commands[0].Equals("d", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 2)
                        {
                            Console.WriteLine("You need to pass <mailbox id> parameter");
                        }
                        else
                        {
                            client.DeleteMailbox(client.Mailboxes[int.Parse(commands[1])].Name);

                            loadCurMailbox(client);
                        }
                    }
                    else if (commands[0].Equals("fa", StringComparison.CurrentCultureIgnoreCase))
                    {
                        client.ListAllMessages();

                        loadMessages(client);
                    }
                    else if (commands[0].Equals("fd", StringComparison.CurrentCultureIgnoreCase))
                    {
                        client.ListDeletedMessages();

                        loadMessages(client);
                    }
                    else if (commands[0].Equals("fn", StringComparison.CurrentCultureIgnoreCase))
                    {
                        client.ListNewMessages();

                        loadMessages(client);
                    }
                    else if (commands[0].Equals("fr", StringComparison.CurrentCultureIgnoreCase))
                    {
                        client.ListRecentMessages();

                        loadMessages(client);
                    }
                    else if (commands[0].Equals("fu", StringComparison.CurrentCultureIgnoreCase))
                    {
                        client.ListUnseenMessages();

                        loadMessages(client);
                    }
                    else if (commands[0].Equals("md", StringComparison.CurrentCultureIgnoreCase))
                    {
                        client.MarkMessageDeleted(client.Messages[msgnum].UID);

                        loadCurMailbox(client);
                    }
                    else if (commands[0].Equals("pu", StringComparison.CurrentCultureIgnoreCase))
                    {
                        client.PurgeMessages();

                        loadCurMailbox(client);
                        loadMessages(client);
                    }
                    else if (commands[0].Equals("r", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 2)
                        {
                            Console.WriteLine("You need to pass <filename> parameter");
                        }
                        else
                        {
                            client.ReceiveFile(client.Messages[msgnum].UID, commands[1]);

                            Console.WriteLine("The message has been received successfully.");
                        }
                    }
                    else if (commands[0].Equals("po", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 2)
                        {
                            Console.WriteLine("You need to pass <filename> parameter");
                        }
                        else
                        {
                            client.PostFile(commands[1], 0, "");

                            loadMessages(client);
                            Console.WriteLine("The message has been posted successfully.");
                        }
                    }
                    else if (commands[0].Equals("h", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (client.Messages.Count > msgnum)
                        {
                            Console.WriteLine("From:    " + client.Messages[msgnum].From);
                            Console.WriteLine("To:      " + client.Messages[msgnum].SentTo);
                            Console.WriteLine("Subject: " + client.Messages[msgnum].Subject);
                            Console.WriteLine("Date:    " + client.Messages[msgnum].Date);
                            Console.WriteLine("Size:    " + client.Messages[msgnum].Size);
                        }
                        else
                        {
                            Console.WriteLine("No info with active message number");
                        }
                    }
                    else if (commands[0].Equals("l", StringComparison.CurrentCultureIgnoreCase))
                    {
                        client.ListMailboxes();

                        loadMailboxes(client);
                    }
                    else if (commands[0].Equals("n", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (msgnum < client.Messages.Count - 1)
                        {
                            msgnum++;
                        }
                        else
                        {
                            Console.WriteLine("The active letter cannot be assigned to switch to the next letter, since the current letter is the last one.");
                        }
                    }
                    else if (commands[0].Equals("p", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (msgnum > 0)
                        {
                            msgnum--;
                        }
                        else
                        {
                            Console.WriteLine("The active letter cannot be assigned to switch to the previous letter, since the current letter is the first.");
                        }
                    }
                    else if (commands[0].Equals("pnum", StringComparison.CurrentCultureIgnoreCase))
                    {
                        Console.WriteLine(msgnum);
                    }
                    else if (commands[0].Equals("bye", StringComparison.CurrentCultureIgnoreCase) || commands[0].Equals("q", StringComparison.CurrentCultureIgnoreCase) || commands[0].Equals("quit", StringComparison.CurrentCultureIgnoreCase))
                    {
                        client.Disconnect();
                        break;
                    }
                    else if (commands[0].Equals("?", StringComparison.CurrentCultureIgnoreCase) || commands[0].Equals("help", StringComparison.CurrentCultureIgnoreCase) || commands[0].Equals("man", StringComparison.CurrentCultureIgnoreCase))
                    {
                        displayCommands();
                    }
                    else
                    {
                        int newNumb;
                        if (int.TryParse(commands[0], out newNumb))
                        {
                            if (newNumb >= 0 && newNumb < client.Messages.Count)
                            {
                                msgnum = newNumb;
                            }
                            else
                            {
                                Console.WriteLine("Not valid number.");
                            }
                        }
                        else
                        {
                            Console.WriteLine("Command not recognized. Choose from these:\n");
                            displayCommands();
                        }
                    }
                }
                else
                {
                    Console.WriteLine("Command not recognized. Choose from these:\n");
                    displayCommands();
                }
            }

            Console.WriteLine();
            confirmExit();
        }
        catch (Exception ex)
        {
            Console.WriteLine("Error: " + ex.Message);
        }
    }
}





class ConsoleDemo
{
  /// <summary>
  /// Takes a list of switch arguments or name-value arguments and turns it into a dictionary.
  /// </summary>
  public static System.Collections.Generic.Dictionary<string, string> ParseArgs(string[] args)
  {
    System.Collections.Generic.Dictionary<string, string> dict = new System.Collections.Generic.Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // Add a key to the dictionary for each argument.
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/", then it is a value.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Save the value and skip the next entry in the list of arguments.
          dict.Add(args[i].ToLower().TrimStart('/'), args[i + 1]);
          i++;
        }
        else
        {
          // If the next argument starts with a "/", then we assume the current one is a switch.
          dict.Add(args[i].ToLower().TrimStart('/'), "");
        }
      }
      else
      {
        // If the argument does not start with a "/", store the argument based on the index.
        dict.Add(i.ToString(), args[i].ToLower());
      }
    }
    return dict;
  }
  /// <summary>
  /// Asks for user input interactively and returns the string response.
  /// </summary>
  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}