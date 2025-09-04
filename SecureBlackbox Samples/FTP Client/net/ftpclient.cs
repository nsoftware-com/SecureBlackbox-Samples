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

class ftpclient
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
            "  ftpclient -- SecureBlackbox FTPClient Demo Application\n\n" +
            "SYNOPSIS\n" +
            "  ftpclient <host> <port> [-username username] [-password password] [-tlsmode tlsmode]\n\n" +
            "DESCRIPTION\n" +
            "  This sample illustrates basic FTP client operations.\n\n" +
            "  The options are as follows:\n\n" +
            "  host        The local host of serve (Required).\n\n" +
            "  port        The port of server (Required).\n\n" +
            "  -username   The user identifier to use for login.\n\n" +
            "  -password   The password to log in.\n\n" +
            "  -tlsmode    The TLS mode. Enter the corresponding string. Valid values: none, explicit, implicit.\n\n" +
            "EXAMPLES\n" +
            "  ftpclient localhost 21\n\n" +
            "  ftpclient localhost 12345 -username testuser -password pass -tlsmode implicit\n"
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
        Console.WriteLine("?         cd        man       quit    \n" +
                           "ascii     get       mkdir     rm      \n" +
                           "binary    help      put       rmdir   \n" +
                           "bye       ls        pwd       verbose");
    }

    private static void confirmExit()
    {
        Console.WriteLine("Press Enter to exit the demo.");
        Console.ReadLine();
    }

    private static bool verbose = false;
    private static FTPClient client;

    private static void _client_OnControlReceive(object sender, FTPClientControlReceiveEventArgs e)
    {
        if (verbose) Console.WriteLine(e.TextLine);
    }

    private static void _client_OnControlSend(object sender, FTPClientControlSendEventArgs e)
    {
        if (verbose) Console.WriteLine(e.TextLine);
    }

    private static void _client_OnListEntry(object sender, FTPClientListEntryEventArgs e)
    {
        Console.WriteLine("  " + client.CurrentListEntry.UnparsedName);
    }

    private static void _client_OnTLSCertValidate(object sender, FTPClientTLSCertValidateEventArgs e)
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

        client = new FTPClient();
        String username = "";

        try
        {
            client.OnControlReceive += _client_OnControlReceive;
            client.OnControlSend += _client_OnControlSend;
            client.OnListEntry += _client_OnListEntry;
            client.OnTLSCertValidate += _client_OnTLSCertValidate;


            client.TLSSettings.AutoValidateCertificates = false;

            if (optext(args, "-username"))
            {
                username = optval(args, "-username");
                client.Username = username;
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

            Console.WriteLine("Connecting to ftp://" + username + "@" + args[0] + ":" + args[1]);

            client.Connect(args[0], int.Parse(args[1]));

            // main loop to check for commands
            while (true)
            {
                var commands = waitCommand("ftp>");

                if (commands.Count > 0)
                {
                    if (commands[0].Equals("ascii", StringComparison.CurrentCultureIgnoreCase))
                    {
                        client.TransferType = FTPClientTransferTypes.cttText;
                        Console.WriteLine("Transfer mode text.");
                    }
                    else if (commands[0].Equals("binary", StringComparison.CurrentCultureIgnoreCase))
                    {
                        client.TransferType = FTPClientTransferTypes.cttBinary;
                        Console.WriteLine("Transfer mode binary.");
                    }
                    else if (commands[0].Equals("put", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 3)
                        {
                            Console.WriteLine("You need to pass two parameters: <local file> and <remote file>");
                        }
                        else
                        {
                            client.UploadFile(commands[1], commands[2]);
                            Console.WriteLine("Uploaded file: " + commands[1] + " -> " + commands[2]);
                        }
                    }
                    else if (commands[0].Equals("get", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 3)
                        {
                            Console.WriteLine("You need to pass two parameters: <remote file> and <local file>");
                        }
                        else
                        {
                            client.DownloadFile(commands[1], commands[2]);
                            Console.WriteLine("Downloaded file: " + commands[1] + " -> " + commands[2]);
                        }
                    }
                    else if (commands[0].Equals("cd", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 2)
                        {
                            Console.WriteLine("You need to pass <new dir> parameter");
                        }
                        else
                        {
                            client.ChangeDir(commands[1]);
                            Console.WriteLine("Changed directory: " + commands[1]);
                        }
                    }
                    else if (commands[0].Equals("mkdir", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 2)
                        {
                            Console.WriteLine("You need to pass <new dir> parameter");
                        }
                        else
                        {
                            client.MakeDir(commands[1]);
                            Console.WriteLine("Created directory: " + commands[1]);
                        }
                    }
                    else if (commands[0].Equals("pwd", StringComparison.CurrentCultureIgnoreCase))
                    {
                        Console.WriteLine("Current directory: " + client.GetCurrentDir());
                    }
                    else if (commands[0].Equals("ls", StringComparison.CurrentCultureIgnoreCase))
                    {
                        Console.WriteLine("Listing " + client.GetCurrentDir());
                        client.ListDir(true, true);
                    }
                    else if (commands[0].Equals("rm", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 2)
                        {
                            Console.WriteLine("You need to pass <file name> parameter");
                        }
                        else
                        {
                            client.DeleteFile(commands[1]);
                            Console.WriteLine("Deleted file: " + commands[1]);
                        }
                    }
                    else if (commands[0].Equals("rmdir", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 2)
                        {
                            Console.WriteLine("You need to pass <dir name> parameter");
                        }
                        else
                        {
                            client.DeleteDir(commands[1]);
                            Console.WriteLine("Deleted directory: " + commands[1]);
                        }
                    }
                    else if (commands[0].Equals("verbose", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (verbose)
                        {
                            verbose = false;
                            Console.WriteLine("Verbose mode off.");
                        }
                        else
                        {
                            verbose = true;
                            Console.WriteLine("Verbose mode on.");
                        }
                    }
                    else if (commands[0].Equals("bye", StringComparison.CurrentCultureIgnoreCase) || commands[0].Equals("exit", StringComparison.CurrentCultureIgnoreCase) || commands[0].Equals("quit", StringComparison.CurrentCultureIgnoreCase))
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