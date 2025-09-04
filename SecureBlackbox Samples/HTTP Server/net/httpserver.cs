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
using _CA;
using nsoftware.SecureBlackbox;
using SBMath;

class httpserver
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
            "  httpserver -- SecureBlackbox HTTPServer Demo Application\n\n" +
            "SYNOPSIS\n" +
            "  httpserver <listening_port> [-cert certificate_file] [-certpass certificate_password]\n\n" +
            "DESCRIPTION\n" +
            "  HTTPServer demonstrates the usage of HTTPServer from SecureBlackbox.\n" +
            "  The options are as follows:\n\n" +
            "  -cert           The certificate used in http server.\n\n" +
            "  -certpass       The password for the certificate.\n\n" +
            "EXAMPLES\n" +
            "  httpserver 80 \n\n" +
            "  httpserver 8080 -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
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

    private static HTTPServer server;

    private static void _server_OnGetRequest(object sender, HTTPServerGetRequestEventArgs e)
    {
        Console.WriteLine("Request received (page: " + e.URI + ")");

        try
        {
            server.SetResponseStatus(e.ConnectionID, 200);
            server.SetResponseString(e.ConnectionID,
                    "<html><head><title>SecureBlackbox HTTP server</title></head><body><h1>Hello there!</h1></body></html>",
                    "text/html", "");

            e.Handled = true;
        }
        catch (Exception ex)
        {
            Console.WriteLine("Error: " + ex.Message);
        }
    }

    static void Main(string[] args)
    {
        if (args.Length < 1)
        {
            displayHelp("listening_port is required.");
            return;
        }

        server = new HTTPServer();
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
            server.OnGetRequest += _server_OnGetRequest;


            if (optext(args, "-cert"))
            {
                CertificateManager cm = new CertificateManager();
                cm.ImportFromFile(optval(args, "-cert"), optval(args, "-certpass"));
                server.TLSServerChain.Add(cm.Certificate);
            }

            server.Start();

            Console.WriteLine("HTTP server started on port " + server.Port + ". Press enter to stop server.");
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