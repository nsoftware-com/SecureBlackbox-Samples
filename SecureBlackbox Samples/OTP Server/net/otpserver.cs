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

class otpserver
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
                "  otpserver -- SecureBlackbox OTPServer Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  otpserver <-alg algorithm> [-secret key_secret] [-pass password] [-interval interval] [-delta delta]\n\n" +
                "DESCRIPTION\n" +
                "  This sample acts as a basic One-Time-Password protocol server. \n" +
                "  The options are as follows:\n\n" +
                "  -alg       The otp algorithm (Required). Valid values:\n\n" +
                "                0 - HOTP\n" +
                "                1 - TOTP\n\n" +
                "  -secret     The key secret (Required).\n\n" +
                "  -pass       The verifiable password (Required).\n\n" +
                "  -interval   The interval value.\n\n" +
                "  -delta      The delta.\n\n" +
                "EXAMPLES\n" +
                "  otpserver -alg 0 -secret testsecret -pass 0334587632\n\n" +
                "  otpserver -alg 1 -secret testsecret -pass 0334587632 -interval 56 -delta 4\n"
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
        if (args.Length == 0)
        {
            displayHelp("");
            return;
        }

        OTPServer server = new OTPServer();
        int algorithm;
        int interval = 0;
        String secret;
        String pass;

        try
        {
            if (optext(args, "-alg"))
            {
                algorithm = int.Parse(optval(args, "-alg"));
            }
            else
            {
                displayHelp("-alg is required.");
                return;
            }

            if (optext(args, "-secret"))
            {
                secret = optval(args, "-secret");
            }
            else
            {
                displayHelp("-secret is required.");
                return;
            }

            if (optext(args, "-pass"))
            {
                pass = optval(args, "-pass");
            }
            else
            {
                displayHelp("-pass is required.");
                return;
            }

            if (optext(args, "-interval"))
            {
                interval = int.Parse(optval(args, "-interval"));
            }

            if (optext(args, "-delta"))
            {
                server.Delta = int.Parse(optval(args, "-delta"));
            }
       
            bool res;
            switch (algorithm)
            {
                case 0:
                    res = server.IsHOTPPasswordValid(Encoding.UTF8.GetBytes(secret), pass.Length, interval, pass);
                    break;
                case 1:
                    res = server.IsTOTPPasswordValid(Encoding.UTF8.GetBytes(secret), pass.Length, interval, "SHA256", pass);
                    break;
                default:
                    displayHelp("Invalid alg value.");
                    return;
            }

            if (res)
                Console.WriteLine("Password is valid");

            else
                Console.WriteLine("Password is not valid");
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