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
using SBMath;

class tlsclient
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
            "  tlsclient -- SecureBlackbox TLSClient Demo Application\n\n" +
            "SYNOPSIS\n" +
            "  tlsclient <host> <port> [-tlsmode tlsmode]\n\n" +
            "DESCRIPTION\n" +
            "  This sample illustrates basic TLS client operations.\n\n" +
            "  The options are as follows:\n\n" +
            "  host        The local host of serve (Required).\n\n" +
            "  port        The port of server (Required).\n\n" +
            "  -tlsmode    The TLS mode. Enter the corresponding string. Valid values: none, implicit.\n\n" +
            "EXAMPLES\n" +
            "  tlsclient localhost 21\n\n" +
            "  tlsclient localhost 12345 -tlsmode implicit\n"
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
        Console.WriteLine("  send <message>        send a letter to the server and receive a response");
        Console.WriteLine("  q                     quit");
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

    private static TLSClient client;

    private static void _client_OnTLSCertValidate(object sender, TLSClientTLSCertValidateEventArgs e)
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

        client = new TLSClient();
        try
        {
            client.OnTLSCertValidate += _client_OnTLSCertValidate;

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

            client.SocketSettings.Timeout = 4000;
            client.TLSSettings.AutoValidateCertificates = false;

            client.Connect(args[0], int.Parse(args[1]));

            Console.WriteLine("Connected successfully!");

            // main loop to check for commands
            while (true)
            {
                var commands = waitCommand("tls>");

                if (commands.Count > 0)
                {
                    if (commands[0].Equals("send", StringComparison.CurrentCultureIgnoreCase))
                    {
                        if (commands.Count < 2)
                        {
                            Console.WriteLine("You need to pass <message> parameter");
                        }
                        else
                        {
                            client.SendText(commands[1]);
                            Console.WriteLine("C->S: " + commands[1]);
                            int maxPartSize = 1000;
                            Console.Write("S->C: ");
                            client.ReceiveData(maxPartSize);
                            while (client.OutputString.Length == 0)
                            {
                                client.ReceiveData(maxPartSize);
                            }
                            Console.WriteLine(client.OutputString);
                        }
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