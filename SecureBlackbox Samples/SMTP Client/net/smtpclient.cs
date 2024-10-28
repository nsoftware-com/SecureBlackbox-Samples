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
using System.IO;
using System.Runtime.InteropServices;
using nsoftware.SecureBlackbox;

class smtpclient
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
                "  smtpclient -- SecureBlackbox SMTPClient Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  smtpclient <host> <port> <from> <sender> <to> <receiver> <subject> <priority> [-username username]\n" +
                "            [-password password] [-tlsmode tlsmode] [-plain plain_file] [-html html_file]\n\n" +
                "DESCRIPTION\n" +
                "  This sample shows how to compose an e-mail message. Also, it's possible to sign and/or encrypt the message with certificates.\n\n" +
                "  The options are as follows:\n\n" +
                "  host        The name or address of a mail server (Required).\n\n" +
                "  port        The port of a mail server (Required).\n\n" +
                "  from        The sender mail address (Required).\n\n" +
                "  sender      The sender name (Required).\n\n" +
                "  to          The receiver mail address (Required).\n\n" +
                "  receiver    The receiver name (Required).\n\n" +
                "  subject     The letter subject (Required).\n\n" +
                "  priority    The priority of letter. Enter the corresponding number from 0 (the lowest) to 4 (the highest) (Required).\n\n" +
                "  -username   The user identifier for the mailbox.\n\n" +
                "  -password   The password for the mailbox user.\n\n" +
                "  -tlsmode    The TLS mode. Enter the corresponding string. Valid values: none, explicit, implicit.\n\n" +
                "  -plain      The file with plain text message.\n\n" +
                "  -html       The file with html text message.\n\n" +
                "EXAMPLES\n" +
                "  smtpclient mail.local 995 Sbb@mail.com SbbTeam user@mail.com User \"test example\" 2 -plain C:\\test.txt\n\n" +
                "  smtpclient mail.local 12345 Sbb@mail.com SbbTeam user@mail.com User \"test example\" 3 -username testuser -password pass -tlsmode implicit\n"
        );

        if (errMes.Length > 0)
        {
            Console.WriteLine("Error: " + errMes);
            Console.WriteLine();
        }

        confirmExit();
    }

    private static void confirmExit()
    {
        Console.WriteLine("Press Enter to exit the demo.");
        Console.ReadLine();
    }

    static void Main(string[] args)
    {
        if (args.Length < 8)
        {
            displayHelp("Required parameters are not specified.");
            return;
        }

        MailWriter writer = new MailWriter();
        SMTPClient client = new SMTPClient();

        try
        {
            writer.From.Add(new MailAddress(args[3], args[2]));
            writer.SendTo.Add(new MailAddress(args[5], args[4]));

            writer.Message.Subject = args[6];

            writer.Message.Priority = (MailPriorities)int.Parse(args[7]);
            if (optext(args, "-plain"))
            {
                writer.Message.PlainText = File.ReadAllText(optval(args, "-plain"));
            }
            if (optext(args, "-html"))
            {
                writer.Message.HtmlText = File.ReadAllText(optval(args, "-html"));
            }

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

            client.SendBytes(writer.SaveToBytes());

            Console.WriteLine("Message has been sent successfully.\n");

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