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

class kmipserver
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
            "  kmipserver -- SecureBlackbox KMIPServer Demo Application\n\n" +
            "SYNOPSIS\n" +
            "  kmipserver <listening_port> <-storage storage_file> [-cert certificate_file] [-certpass certificate_password]\n\n" +
            "          [-users users_file] [-userspass users_password] [-tlsmode tlsmode] [-usehttp]\n\n" +
            "DESCRIPTION\n" +
            "  KMIPServer demonstrates the usage of KMIPServer from SecureBlackbox.\n" +
            "  The options are as follows:\n\n" +
            "  -storage        The storage used in kmip server (Required).\n\n" +
            "  -cert           The certificate used in kmip server as CA certificate.\n\n" +
            "  -certpass       The password for the certificate.\n\n" +
            "  -users          The users used in kmip server.\n\n" +
            "  -userspass      The password for the users file.\n\n" +
            "  -tlsmode        The TLS mode. Enter the corresponding string. Valid values: none, implicit.\n\n" +
            "  -usehttp        Whether to use http.\n\n" +
            "EXAMPLES\n" +
            "  kmipserver 80 -storage C:\\storage.tmp\n\n" +
            "  kmipserver 8080 -storage C:\\storage.tmp -cert C:\\certs\\mycert.pfx -certpass mypassword -usehttp\n"
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

    private static KMIPServer server;

    private static void _server_OnOperationAttempt(object sender, KMIPServerOperationAttemptEventArgs e)
    {
        Console.WriteLine("Request for " + e.Operation + " from " + e.Username);
        e.Reject = false;
    }

    private static void _server_OnKMIPAuthAttempt(object sender, KMIPServerKMIPAuthAttemptEventArgs e)
    {
        if (server.Users.Count == 0)
            e.Accept = true;
    }

    private static void _server_OnError(object sender, KMIPServerErrorEventArgs e)
    {
        Console.WriteLine("Error code: " + e.ErrorCode.ToString() + ". Description: " + e.Description);
    }

    static void Main(string[] args)
    {
        if (args.Length < 1)
        {
            displayHelp("listening_port is required.");
            return;
        }

        server = new KMIPServer();
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
            server.OnOperationAttempt += _server_OnOperationAttempt;
            server.OnKMIPAuthAttempt += _server_OnKMIPAuthAttempt;
            server.OnError += _server_OnError;

            if (optext(args, "-storage"))
            {
                server.StorageFileName = optval(args, "-storage");
            }
            else
            {
                displayHelp("-storage is required.");
                return;
            }

            server.TLSSettings.TLSMode = SSLModes.smNoTLS;
            if (optext(args, "-tlsmode"))
            {
                String tlsmode = optval(args, "-tlsmode");
                if (tlsmode.Equals("none", StringComparison.CurrentCultureIgnoreCase))
                {
                    server.TLSSettings.TLSMode = SSLModes.smNoTLS;
                }
                else if (tlsmode.Equals("implicit", StringComparison.CurrentCultureIgnoreCase))
                {
                    server.TLSSettings.TLSMode = SSLModes.smImplicitTLS;
                }
            }

            if (optext(args, "-cert"))
            {
                CertificateManager cm = new CertificateManager();
                cm.ImportFromFile(optval(args, "-cert"), optval(args, "-certpass"));
                server.CACertificate = cm.Certificate;
            }

            if (optext(args, "-users"))
            {
                UserManager um = new UserManager();
                um.ImportFromFile(optval(args, "-users"), optval(args, "-userspass"), true);
                for (int x = 0; x < um.Users.Count; x++)
                    server.Users.Add(um.Users[x]);
            }

            if (optext(args, "-usehttp"))
            {
                server.UseHTTP = true;
            }

            server.Start();

            Console.WriteLine("KMIP server started on port " + server.Port + ". Press enter to stop server and exit.");
            Console.ReadLine();

            server.Stop();

            Console.WriteLine("Server stopped.\n");
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