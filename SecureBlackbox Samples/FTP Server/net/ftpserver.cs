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

class ftpserver
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
                "  ftpserver -- SecureBlackbox FTPServer Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  ftpserver <listening_port> [-users users_file] [-userspass users_password] [-cert certificate_file]\n" +
                "              [-certpass certificate_password] [-tlsmode tlsmode] [-allowanon]\n\n" +
                "DESCRIPTION\n" +
                "  FTPServer demonstrates the usage of FTPServer from SecureBlackbox.\n" +
                "  The options are as follows:\n\n" +
                "  -users          An file with users information.\n\n" +
                "  -userspass      The password for users file.\n\n" +
                "  -cert           The certificate used in ftp server.\n\n" +
                "  -certpass       The password for the certificate.\n\n" +
                "  -tlsmode        The TLS mode. Enter the corresponding string. Valid values: none, explicit, implicit.\n\n" +
                "  -allowanon      Whether to allow connection from anonymous user.\n\n" +
                "EXAMPLES\n" +
                "  ftpserver 80 \n\n" +
                "  ftpserver 8080 -users C:\\ftpserver\\users.dat -cert C:\\certs\\mycert.pfx -certpass mypassword \n" +
                "             -tlsmode implicit -allowanon \n"
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

    private static FTPServer server;

    private static void _server_OnAuthAttempt(object sender, FTPServerAuthAttemptEventArgs e)
    {
        if (e.Allow)
        {
            Console.WriteLine("Access granted to user " + e.Username);
        }
        else
        {
            Console.WriteLine("Access denied for user " + e.Username);
        }
    }

    private static void _server_OnConnect(object sender, FTPServerConnectEventArgs e)
    {
        Console.WriteLine("Client connected from " + e.RemoteAddress + ":" + e.Port);
    }

    static void Main(string[] args)
    {
        if (args.Length < 1)
        {
            displayHelp("listening_port is required.");
            return;
        }

        FTPServer server = new FTPServer();
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



            server.TLSSettings.TLSMode = SSLModes.smNoTLS;
            if (optext(args, "-tlsmode"))
            {
                String tlsmode = optval(args, "-tlsmode");
                if (tlsmode.Equals("none", StringComparison.CurrentCultureIgnoreCase))
                {
                    server.TLSSettings.TLSMode = SSLModes.smNoTLS;
                }
                else if (tlsmode.Equals("explicit", StringComparison.CurrentCultureIgnoreCase))
                {
                    server.TLSSettings.TLSMode = SSLModes.smExplicitTLS;
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
                server.TLSServerChain.Add(cm.Certificate);
            }
            else
            {
                if (server.TLSSettings.TLSMode != SSLModes.smNoTLS)
                {
                    displayHelp("The server cannot support TLS without a valid server certificate. Please provide a certificate file via the cert and certpass parameters.");
                    return;
                }
            }

            if (optext(args, "-users"))
            {
                UserManager um = new UserManager();
                um.ImportFromFile(optval(args, "-users"), optval(args, "-userspass"), true);
                for (int x = 0; x < um.Users.Count; x++)
                    server.Users.Add(um.Users[x]);
            }

            if (optext(args, "-allowanon"))
            {
                server.AllowAnonymous = true;
            }

            server.Start();

            Console.WriteLine("FTP server started on port " + server.Port + ". Press enter to stop server.");
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