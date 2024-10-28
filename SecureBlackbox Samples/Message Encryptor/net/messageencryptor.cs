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

class messageencryptor
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
                "  messageencryptor -- SecureBlackbox MessageEncryptor Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  messageencryptor <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password] [-encalg encryption_algorithm] \n\n" +
                "DESCRIPTION\n" +
                "  MessageEncryptor demonstrates the usage of MessageEncryptor from SecureBlackbox.\n" +
                "  Used to create encrypted ('enveloped') PKCS#7 messages. \n\n" +
                "  The options are as follows:\n\n" +
                "  -input        An input file to encrypt (Required).\n\n" +
                "  -output       Where the encrypted file will be saved (Required).\n\n" +
                "  -cert         The certificate used to encrypt file (Required).\n\n" +
                "  -certpass     The password for the encryption certificate.\n\n" +
                "  -encalg       The encryption algorithm. Valid values: 3DES, RC4, RC2, AES128, AES192, AES256, Twofish128 \n\n" +
                "EXAMPLES\n" +
                "  messageencryptor -input C:\\pkcs7\\helloworld.txt -output C:\\pkcs7\\enc.pkcs7 -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n" +
                "  messageencryptor -input C:\\pkcs7\\helloworld.txt -output C:\\pkcs7\\enc.pkcs7 -cert C:\\certs\\mycert.pfx -certpass mypassword -alg AES128 \n"
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

        MessageEncryptor encryptor = new MessageEncryptor();
        CertificateManager cm = new CertificateManager();

        try
        {
            if (optext(args, "-input"))
            {
                encryptor.InputFile = optval(args, "-input");
            }
            else
            {
                displayHelp("-input is required.");
                return;
            }

            if (optext(args, "-output"))
            {
                encryptor.OutputFile = optval(args, "-output");
            }
            else
            {
                displayHelp("-output is required.");
                return;
            }
            if (optext(args, "-cert"))
            {
                cm.ImportFromFile(optval(args, "-cert"), optval(args, "-certpass"));
                encryptor.EncryptionCertificates.Add(cm.Certificate);
            }
            else
            {
                displayHelp("-cert is required.");
                return;
            }

            if (optext(args, "-encalg"))
            {
                encryptor.EncryptionAlgorithm = optval(args, "-encalg");
            }

            encryptor.Encrypt();

            Console.WriteLine("The file successfully encrypted.\n");

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