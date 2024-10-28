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

class sftpclient
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
            "  sftpclient -- SecureBlackbox SFTPClient Demo Application\n\n" +
            "SYNOPSIS\n" +
            "  sftpclient <host> <port> [-username username] [-password password]\n\n" +
            "DESCRIPTION\n" +
            "  This sample illustrates basic FTP client operations.\n\n" +
            "  The options are as follows:\n\n" +
            "  host        The local host of serve (Required).\n\n" +
            "  port        The port of server (Required).\n\n" +
            "  -username   The user identifier to use for login.\n\n" +
            "  -password   The password to log in.\n\n" +
            "EXAMPLES\n" +
            "  sftpclient localhost 21\n\n" +
            "  sftpclient localhost 12345 -username testuser -password pass\n"
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
                          "get       mkdir     rm      \n" +
                          "help      put       rmdir   \n" +
                          "bye       ls        pwd       verbose");
    }

    private static void confirmExit()
    {
        Console.WriteLine("Press Enter to exit the demo.");
        Console.ReadLine();
    }

    private static bool verbose = false;
    private static SFTPClient client;

    private static void _client_OnError(object sender, SFTPClientErrorEventArgs e)
    {
        Console.WriteLine("Error: " + e.ErrorCode + " (" + e.Description + ")");
    }

    private static void _client_OnFileOperation(object sender, SFTPClientFileOperationEventArgs e)
    {
        if (verbose)
        {
            String mes;
            switch (e.Operation)
            {
                case Constants.cffoDownloadFile:
                    mes = "Download file.";
                    break;
                case Constants.cffoUploadFile:
                    mes = "Upload file.";
                    break;
                case Constants.cffoDeleteFile:
                    mes = "Delete file.";
                    break;
                case Constants.cffoMakeDir:
                    mes = "Make directory.";
                    break;
                default:
                    mes = "Unknown.";
                    break;
            }

            if (e.LocalPath.Length > 0)
                mes = mes + " Local path: " + e.LocalPath;

            if (e.RemotePath.Length > 0)
                mes = mes + " Remote path: " + e.RemotePath;

            Console.WriteLine(mes);
        }
    }

    private static void _client_OnListEntry(object sender, SFTPClientListEntryEventArgs e)
    {
        Console.WriteLine("  " + client.CurrentListEntry.Name);
    }

    private static void _client_OnUnknownKeyReceived(object sender, SFTPClientUnknownKeyReceivedEventArgs e)
    {
        e.Action = 2;
    }

    static void Main(string[] args)
    {
        if (args.Length < 2)
        {
            displayHelp("host and port is required.");
            return;
        }

        client = new SFTPClient();
        String username = "";

        try
        {
            client.OnError += _client_OnError;
            client.OnFileOperation += _client_OnFileOperation;
            client.OnListEntry += _client_OnListEntry;
            client.OnUnknownKeyReceived += _client_OnUnknownKeyReceived;

            if (optext(args, "-username"))
            {
                username = optval(args, "-username");
                client.Username = username;
            }

            if (optext(args, "-password"))
            {
                client.Password = optval(args, "-password");
            }

            Console.WriteLine("Connecting to sftp://" + username + "@" + args[0] + ":" + args[1]);

            client.Connect(args[0], int.Parse(args[1]));

            // main loop to check for commands
            while (true)
            {
                var commands = waitCommand("sftp>");

                if (commands.Count > 0)
                {
                    if (commands[0].Equals("put", StringComparison.CurrentCultureIgnoreCase))
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