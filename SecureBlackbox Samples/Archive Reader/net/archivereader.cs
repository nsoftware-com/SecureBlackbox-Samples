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

class archivereader
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
                "  archivereader -- SecureBlackbox ArchiveReader Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  archivereader <-arctype archive_type> <-input input_file> [-output output_path] [-pass decryption_password]\n\n" +
                "DESCRIPTION\n" +
                "  ArchiveReader demonstrates the usage of ArchiveReader from SecureBlackbox.\n" +
                "  Used to extract files from archive.\n\n" +
                "  The options are as follows:\n\n" +
                "  -arctype      The type of archive (Required). Valid values:\n\n" +
                "                  1 - AFT_ZIP\n" +
                "                  2 - AFT_GZIP\n" +
                "                  3 - AFT_BZIP_2\n" +
                "                  4 - AFT_TAR\n" +
                "                  5 - AFT_TAR_GZIP\n" +
                "                  6 - AFT_TAR_BZIP_2\n\n" +
                "  -input        An input archive file (Required).\n\n" +
                "  -output       Where the extracted files will be saved.\n\n" +
                "  -pass         The password for the encrypted archive.\n\n" +
                "EXAMPLES\n" +
                "  archivereader -arctype 1 -input C:\\archive\\helloworld.zip -output C:\\archive \n\n" +
                "  archivereader -arctype 5 -input C:\\archive\\helloworld.tar -pass mypassword \n"
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

        ArchiveReader archive = new ArchiveReader();
        string input;
        int arctype;

        if (optext(args, "-arctype"))
        {
            try
            {
                arctype = int.Parse(optval(args, "-arctype"));
            }
            catch (Exception e)
            {
                displayHelp("-arctype invalid value.");
                return;
            }
        }
        else
        {
            displayHelp("-arctype is required.");
            return;
        }

        if (optext(args, "-input"))
        {
            input = optval(args, "-input");
        }
        else
        {
            displayHelp("-input is required.");
            return;
        }

        // Open archive
        try
        {
            // Additional options
            archive.DecryptionPassword = optval(args, "-pass");

            archive.Open(arctype, input);

            Console.WriteLine("List of files in the archive");
            for (int x = 0; x < archive.Files.Count; x++)
            {
                Console.WriteLine("    " + archive.Files[x].FileName);
            }
            Console.WriteLine("");

            if (optext(args, "-output"))
            {
                // Extract all files
                archive.Overwrite = true;
                archive.ExtractAll(optval(args, "-output"));

                Console.WriteLine("All files from archive extracted.\n");
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