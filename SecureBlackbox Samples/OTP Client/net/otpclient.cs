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

class otpclient
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
                "  otpclient -- SecureBlackbox OTPClient Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  otpclient <-alg algorithm> [-secret key_secret] [-len pass_length] [-interval interval]\n\n" +
                "DESCRIPTION\n" +
                "  This sample acts as a basic One-Time-Password protocol client.\n" +
                "  The options are as follows:\n\n" +
                "  -alg       The otp algorithm (Required). Valid values:\n\n" +
                "                0 - HOTP\n" +
                "                1 - TOTP\n\n" +
                "  -secret     The key secret (Required).\n\n" +
                "  -len        The password length.\n\n" +
                "  -interval   The interval value.\n\n" +
                "EXAMPLES\n" +
                "  otpclient -alg 0 -secret testsecret \n\n" +
                "  otpclient -alg 1 -secret testsecret -len 8 -interval 56 \n"
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

        OTPClient client = new OTPClient();
        int algorithm;
        int interval = 0;

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
                client.KeySecret = Encoding.UTF8.GetBytes(optval(args, "-secret"));
            }
            else
            {
                displayHelp("-secret is required.");
                return;
            }

            if (optext(args, "-len"))
            {
                client.PasswordLength = int.Parse(optval(args, "-len"));
            }
            else
            {
                client.PasswordLength = 10;
            }

            if (optext(args, "-interval"))
            {
                interval = int.Parse(optval(args, "-interval"));
            }

            // Calculate password
            switch (algorithm)
            {
                case 0:
                    Console.WriteLine("Calculated password: " + client.GenerateHOTPPassword(interval));
                    break;
                case 1:
                    Console.WriteLine("Calculated password: " + client.GenerateTOTPPassword(interval, "SHA256"));
                    break;
                default:
                    displayHelp("Invalid alg value.");
                    return;
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