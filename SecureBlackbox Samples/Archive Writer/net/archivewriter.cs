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
using System.IO;
using nsoftware.SecureBlackbox;

class archivewriter
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

    private static void displayHelp(String errMes)
    {
        Console.WriteLine(
                "NAME\n" +
                "  archivewriter -- SecureBlackbox ArchiveWriter Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  archivewriter <-arctype archive_type> <-input input1 input2 ....> <-output output_file> [-pass encryption_password] [-enctype encryption_type]\n\n" +
                "DESCRIPTION\n" +
                "  ArchiveWriter demonstrates the usage of ArchiveWriter from SecureBlackbox.\n" +
                "  Used to create archive.\n\n" +
                "  The options are as follows:\n\n" +
                "  -arctype      The type of archive (Required). Valid values:\n\n" +
                "                  1 - AFT_ZIP\n" +
                "                  2 - AFT_GZIP\n" +
                "                  3 - AFT_BZIP_2\n" +
                "                  4 - AFT_TAR\n" +
                "                  5 - AFT_TAR_GZIP\n" +
                "                  6 - AFT_TAR_BZIP_2\n\n" +
                "  -input        List of files or directories compressing to archive (Required).\n\n" +
                "  -output       Where the archive file will be saved (Required).\n\n" +
                "  -pass         The password for the encryption.\n\n" +
                "  -enctype      The encryption type. Valid values: \n\n" +
                "                  0 - AET_DEFAULT\n" +
                "                  1 - AET_NO_ENCRYPTION\n" +
                "                  2 - AET_GENERIC\n" +
                "                  3 - AET_WIN_ZIP\n" +
                "                  4 - AET_STRONG\n\n" +
                "EXAMPLES\n" +
                "  archivewriter -arctype 1 -input C:\\archive\\helloworld.txt -output C:\\archive\\myarchive.zip \n\n" +
                "  archivewriter -arctype 5 -input C:\\archive\\helloworld.txt C:\\archive\\temp -output C:\\archive\\myarchive.tar -enctype 2 -pass mypassword \n"
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

        ArchiveWriter archive = new ArchiveWriter();
        int arctype;
        String output;

        if (optext(args, "-arctype"))
        {
            try
            {
                arctype = int.Parse(optval(args, "-arctype"));
            }
            catch (Exception ex)
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

        if (!optext(args, "-input"))
        {
            displayHelp("-input is required.");
            return;
        }

        if (optext(args, "-output"))
        {
            output = optval(args, "-output");
        }
        else
        {
            displayHelp("-output is required.");
            return;
        }

        try
        {
            // Additional options
            if (optext(args, "-enctype"))
            {
                archive.EncryptionType = (ArchiveWriterEncryptionTypes)int.Parse(optval(args, "-enctype"));
                archive.EncryptionPassword = optval(args, "-pass");
            }

            // Create archive
            archive.CreateNew(arctype);

            // Add input files
            for (int x = 0; x < args.Length; x++)
                if (args[x].Equals("-input", StringComparison.CurrentCultureIgnoreCase) && x != args.Length - 1)
                {
                    for (int i = x + 1; i < args.Length; i++)
                    {
                        if (args[i].StartsWith("-")) break;

                        if (File.Exists(args[i])) // file exists
                        {
                            archive.AddFile(Path.GetFileName(args[i]), args[i]);
                        }
                        else
                        {
                            archive.AddFiles("", args[i], true);
                        }
                    }

                    break;
                }

            Console.WriteLine("List of files added to the archive");
            for (int x = 0; x < archive.Files.Count; x++)
            {
                Console.WriteLine("    " + archive.Files[x].Path);
            }
            Console.WriteLine("");

            // Save archive
            archive.Save(output);

            Console.WriteLine("Archive created.\n");

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