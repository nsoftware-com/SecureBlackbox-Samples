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

class popclient
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
                "  popclient -- SecureBlackbox POPClient Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  popclient <host> <port> [-username username] [-password password] [-tlsmode tlsmode]\n\n" +
                "DESCRIPTION\n" +
                "  This sample how to use POP component to view messages in the mailbox.\n\n" +
                "  The options are as follows:\n\n" +
                "  host        The name or address of a mail server (Required).\n\n" +
                "  port        The port of a mail server (Required).\n\n" +
                "  -username   The user identifier for the mailbox.\n\n" +
                "  -password   The password for the mailbox user.\n\n" +
                "  -tlsmode    The TLS mode. Enter the corresponding string. Valid values: none, explicit, implicit.\n\n" +
                "EXAMPLES\n" +
                "  popclient mail.local 995\n\n" +
                "  popclient mail.local 12345 -username testuser -password pass -tlsmode implicit\n"
        );

        if (errMes.Length > 0)
        {
            Console.WriteLine("Error: " + errMes);
            Console.WriteLine();
        }

        confirmExit();
    }
    private static void displayCommands()
    {
        Console.WriteLine("Mail Commands\n" +
                "l                               listing messages in mbox\n" +
                "h <message number>              print out message headers\n" +
                "f <message number>              print out full message info\n" +
                "d <message number>              delete message\n" +
                "q                               quit, saving unresolved messages in mbox");
    }

    private static void confirmExit()
    {
        Console.WriteLine("Press Enter to exit the demo.");
        Console.ReadLine();
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

    private static POP3Client client;

    private static void _client_OnTLSCertValidate(object sender, POP3ClientTLSCertValidateEventArgs e)
    {
        e.Accept = true;
    }

    static void Main(string[] args)
    {
        if (args.Length < 2)
        {
            displayHelp("host and port is required.");
            return;
        }

        client = new POP3Client();
        try
        {
            client.OnTLSCertValidate += _client_OnTLSCertValidate;

            client.Config("RequestUIDs=True");
            client.TLSSettings.AutoValidateCertificates = false;
            client.TLSSettings.TLSMode = SSLModes.smNoTLS;

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

            client.Connect(args[0], int.Parse(args[1]));

            Console.WriteLine("Connected successfully!");

            // main loop to check for commands
            while (true)
            {
                var commands = waitCommand("pop>");

                if (commands.Count > 0)
                {
                    if (commands[0].Equals("l", StringComparison.CurrentCultureIgnoreCase))
                    {
                        client.ListMessages();

                        if (client.Messages.Count > 0)
                        {
                            for (int i = 0; i < client.Messages.Count; i++)
                            {
                                Console.WriteLine("Uid: " + client.Messages[i].UID);
                                Console.WriteLine("Size: " + client.Messages[i].Size);
                            }
                        }
                        else
                        {
                            Console.WriteLine("No messages on the server.");
                        }
                    }
                    else if (commands[0].Equals("d", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 2)
                        {
                            Console.WriteLine("You need to pass <message number> parameter");
                        }
                        else
                        {
                            client.DeleteMessage(int.Parse(commands[1]));
                            Console.WriteLine("Message deleted");
                        }
                    }
                    else if (commands[0].Equals("h", StringComparison.CurrentCultureIgnoreCase) || commands[0].Equals("f", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 2)
                        {
                            Console.WriteLine("You need to pass <message number> parameter");
                        }
                        else
                        {
                            client.ReceiveMessage(int.Parse(commands[1]));

                            Console.WriteLine("Message info:");
                            Console.WriteLine("From: " + client.Message.From);
                            Console.WriteLine("To: " + client.Message.SendTo);
                            Console.WriteLine("Date: " + client.Message.Date);
                            Console.WriteLine("Subject: " + client.Message.Subject);

                            switch (client.Message.Priority)
                            {
                                case MailPriorities.mpLowest:
                                    Console.WriteLine("Priority: [lowest]");
                                    break;
                                case MailPriorities.mpLow:
                                    Console.WriteLine("Priority: [low]");
                                    break;
                                case MailPriorities.mpNormal:
                                    Console.WriteLine("Priority: [normal]");
                                    break;
                                case MailPriorities.mpHigh:
                                    Console.WriteLine("Priority: [HIGH]");
                                    break;
                                case MailPriorities.mpHighest:
                                    Console.WriteLine("Priority: [HIGHEST]");
                                    break;
                            }
                            if (commands[0].Equals("f", StringComparison.CurrentCultureIgnoreCase))
                            {
                                Console.WriteLine("Plain text: " + client.Message.PlainText + "\n");
                                Console.WriteLine("Html text: " + client.Message.HtmlText + "\n");
                            }
                        }
                    }
                    else if (commands[0].Equals("bye", StringComparison.CurrentCultureIgnoreCase) || commands[0].Equals("q", StringComparison.CurrentCultureIgnoreCase) || commands[0].Equals("quit", StringComparison.CurrentCultureIgnoreCase))
                    {
                        Console.Write("Save changes to inbox");
                        if (Console.ReadLine().StartsWith('n'))
                        {
                            client.Undelete();
                        }
                        client.Disconnect();
                        break;
                    }
                    else if (commands[0].Equals("?", StringComparison.CurrentCultureIgnoreCase) || commands[0].Equals("help", StringComparison.CurrentCultureIgnoreCase) || commands[0].Equals("man", StringComparison.CurrentCultureIgnoreCase))
                    {
                        displayCommands();
                    }
                    else
                    {
                        Console.WriteLine("Command not recognized. Choose from these:\n");
                        displayCommands();
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