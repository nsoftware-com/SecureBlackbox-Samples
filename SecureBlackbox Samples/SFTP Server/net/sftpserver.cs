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
using nsoftware.SecureBlackbox;
using SBMath;

class sftpserver
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

    private static SFTPServer server;

    private static void _server_OnAuthAttempt(object sender, SFTPServerAuthAttemptEventArgs e)
    {
        if (e.Accept)
        {
            Console.WriteLine("Access granted to user " + e.Username);
        }
        else
        {
            Console.WriteLine("Access denied for user " + e.Username);
        }
    }

    private static void _server_OnConnect(object sender, SFTPServerConnectEventArgs e)
    {
        Console.WriteLine("Client connected from " + e.RemoteAddress + ":" + e.RemotePort);
    }

    static void Main(string[] args)
    {
        if (args.Length < 1)
        {
            displayHelp("listening_port is required.");
            return;
        }

        server = new SFTPServer();
        try
        {
            server.Port = int.Parse(args[0]);
        }
        catch (Exception ex)
        {
            displayHelp("Invalid port value");
            return;
        }

        try
        {
            server.OnAuthAttempt += _server_OnAuthAttempt;
            server.OnConnect += _server_OnConnect;


            if (optext(args, "-basedir"))
            {
                server.BaseDir = optval(args, "-basedir");
            }

            if (optext(args, "-key"))
            {
                SSHKeyManager km = new SSHKeyManager();
                km.ImportFromFile(optval(args, "-key"), optval(args, "-keypass"));
                server.ServerKeys.Add(km.Key);
            }

            if (optext(args, "-users"))
            {
                UserManager um = new UserManager();
                um.ImportFromFile(optval(args, "-users"), optval(args, "-userspass"), true);
                for (int x = 0; x < um.Users.Count; x++)
                    server.Users.Add(um.Users[x]);
            }

            server.Start();

            Console.WriteLine("SFTP server started on port " + server.Port + ". Press enter to stop server.");
            Console.ReadLine();

            server.Stop();

            Console.WriteLine("Server stopped.\n");

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