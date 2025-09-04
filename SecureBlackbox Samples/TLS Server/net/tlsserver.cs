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
using System.Text;
using nsoftware.SecureBlackbox;

class tlsserver
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
                "  tlsserver -- SecureBlackbox TLSServer Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  tlsserver <listening_port> [-cert certificate_file] [-certpass certificate_password] [-tlsmode tlsmode]\n\n" +
                "DESCRIPTION\n" +
                "  TLSServer demonstrates the usage of TLSServer from SecureBlackbox.\n" +
                "  The options are as follows:\n\n" +
                "  -cert           The certificate used in tls server.\n\n" +
                "  -certpass       The password for the certificate.\n\n" +
                "  -tlsmode        The TLS mode. Enter the corresponding string. Valid values: none, implicit.\n\n" +
                "EXAMPLES\n" +
                "  tlsserver 80 \n\n" +
                "  tlsserver 8080 -cert C:\\certs\\mycert.pfx -certpass mypassword -tlsmode implicit \n"
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

    private static TLSServer server;

    private static void _server_OnAccept(object sender, TLSServerAcceptEventArgs e)
    {
        e.Accept = true;
        Console.WriteLine("Accepted a new client from " + e.RemoteAddress + ":" + e.RemotePort);
    }

    private static void _server_OnData(object sender, TLSServerDataEventArgs e)
    {
        Console.WriteLine(" [" + e.ConnectionID + "] C->S: " + Encoding.UTF8.GetString(e.Buffer));

        byte[] dst = e.Buffer;
        Array.Reverse(dst);

        try
        {
            server.SendData(e.ConnectionID, dst);
            Console.WriteLine(" [" + e.ConnectionID + "] S->C: " + Encoding.UTF8.GetString(dst));
        }
        catch (Exception ex)
        {
            Console.WriteLine("Error: " + ex.Message);
        }
    }

    private static void _server_OnError(object sender, TLSServerErrorEventArgs e)
    {
        Console.WriteLine(" [" + e.ConnectionID + "] Error " + e.ErrorCode + ": " + e.Description);
    }

    private static void _server_OnTLSEstablished(object sender, TLSServerTLSEstablishedEventArgs e)
    {
        Console.WriteLine(" [" + e.ConnectionID + "] Secure session established");
    }

    private static void _server_OnTLSShutdown(object sender, TLSServerTLSShutdownEventArgs e)
    {
        Console.WriteLine(" [" + e.ConnectionID + "] Secure session closed");
    }

    static void Main(string[] args)
    {
        if (args.Length < 1)
        {
            displayHelp("listening_port is required.");
            return;
        }

        server = new TLSServer();
        try
        {
            server.Port = int.Parse(args[0]);
        }
        catch (Exception ex)
        {
            Console.WriteLine();
            displayHelp("Invalid port value");
            return;
        }

        try
        {
            server.OnAccept += _server_OnAccept;
            server.OnData += _server_OnData;
            server.OnError += _server_OnError;
            server.OnTLSEstablished += _server_OnTLSEstablished;
            server.OnTLSShutdown += _server_OnTLSShutdown;

            if (optext(args, "-cert"))
            {
                CertificateManager cm = new CertificateManager();
                cm.ImportFromFile(optval(args, "-cert"), optval(args, "-certpass"));
                server.TLSServerChain.Add(cm.Certificate);
            }

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

            server.Start();

            Console.WriteLine("TLS server started on port " + server.Port + ". Press enter to stop server.");
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