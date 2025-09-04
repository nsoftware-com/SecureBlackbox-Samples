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

class pkcs11certificatestorage
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
                "  pkcs11certificatestorage -- SecureBlackbox CertificateStorage Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  pkcs11certificatestorage <-storage driver_path> [-pin pin]\n\n" +
                "DESCRIPTION\n" +
                "  This sample illustrates the use of CertificateStorage component to access HSMs via PKCS11 interface. \n\n" +
                "  The options are as follows:\n\n" +
                "  -storage      A path to the pkcs11 driver file (Required).\n\n" +
                "  -pin          The user PIN for the device. If no PIN is provided, the sample won't be signing in.\n\n" +
                "EXAMPLES\n" +
                "  pkcs11certificatestorage -storage C:\\pkcs11\\pkcs11.dll -pin mypassword\n"
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

        try
        {
            CertificateStorage certstorage = new CertificateStorage();
            CertificateStorage certstorage_dop = new CertificateStorage();

            String storageFile = "";
            String pin = "";


            if (optext(args, "-storage"))
            {
                storageFile = optval(args, "-storage");
            }
            else
            {
                displayHelp("-storage is required.");
                return;
            }


            if (optext(args, "-pin"))
            {
                pin = optval(args, "-pin");
            }

            certstorage.Open("pkcs11:///" + storageFile + "?slot=-1");

            string[] slots = certstorage.ListStores().Split("\\r?\\n");

            for (int i = 0; i < slots.Length; i++)
            {
                String desc = slots[i];
                String active = certstorage.Config("PKCS11SlotTokenPresent[" + i + "]");

                if (desc != "")
                {
                    if (active == "True")
                    {
                        Console.WriteLine(desc + ":");

                        certstorage_dop.Open("pkcs11://user:" + pin + "@/" + storageFile + "?slot=" + i);

                        for (int j = 0; j < certstorage_dop.Certificates.Count; j++)
                        {
                            Console.WriteLine("	Subject: " + certstorage_dop.Certificates[j].Subject);
                            Console.WriteLine("	Issuer: " + certstorage_dop.Certificates[j].Issuer);
                            Console.WriteLine("	ValidFrom: " + certstorage_dop.Certificates[j].ValidFrom);
                            Console.WriteLine("	ValidTo: " + certstorage_dop.Certificates[j].ValidTo);
                            Console.WriteLine("	Key: " + certstorage_dop.Certificates[j].KeyAlgorithm + " (" + certstorage_dop.Certificates[j].KeyBits + ")");
                            Console.WriteLine("");
                        }

                        Console.WriteLine("");
                    }
                    else
                    {
                        Console.WriteLine(desc + ": No token ");
                        Console.WriteLine("");
                    }
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