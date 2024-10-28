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

class hashfunction
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
                "  hashfunction -- SecureBlackbox HashFunction Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  hashfunction <-input input_file> [-pass password] [-alg hash_alg] [-encoding encoding_type]\n\n" +
                "DESCRIPTION\n" +
                "  HashFunction demonstrates the usage of HashFunction from SecureBlackbox.\n" +
                "  Used to create an hash from file.\n\n" +
                "  The options are as follows:\n\n" +
                "  -input        An input file to hash (Required).\n\n" +
                "  -pass         The password for derive key.\n\n" +
                "  -alg          The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224, MD5\n\n" +
                "  -encoding     The encoding of hash. Valid values:\n\n" +
                "                  0 - CET_DEFAULT\n" +
                "                  1 - CET_BINARY\n" +
                "                  2 - CET_BASE_64\n" +
                "                  3 - CET_COMPACT\n" +
                "                  4 - CET_JSON\n\n" +
                "EXAMPLES\n" +
                "  hashfunction -input C:\\hash\\helloworld.txt \n\n" +
                "  hashfunction -input C:\\hash\\helloworld.txt -encoding 2 -pass mypassword \n"
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

        HashFunction hash = new HashFunction();
        CryptoKeyManager cm = new CryptoKeyManager();
        String input;

        try
        {
            if (optext(args, "-input"))
            {
                input = optval(args, "-input");
            }
            else
            {
                displayHelp("-input is required.");
                return;
            }

            if (optext(args, "-pass"))
            {
                cm.DeriveKey(128, optval(args, "-pass"), "");

                hash.Key = cm.Key;
            }

            if (optext(args, "-alg"))
            {
                hash.Algorithm = optval(args, "-alg");
            }
            else
            {
                hash.Algorithm = "SHA256";
            }

            if (optext(args, "-encoding"))
            {
                hash.OutputEncoding = (HashFunctionOutputEncodings)int.Parse(optval(args, "-encoding"));
            }
            else
            {
                hash.OutputEncoding = HashFunctionOutputEncodings.cetBase64;
            }

            // Calculate hash
            Console.WriteLine("Calculated hash: " + Encoding.UTF8.GetString(hash.HashFile(input)));
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