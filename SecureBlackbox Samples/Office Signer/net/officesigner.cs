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

class officesigner
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
                "  officesigner -- SecureBlackbox OfficeSigner Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  officesigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password]\n" +
                "             [-sigtype signature_type] [-hashalg hash_algorithm] [-signdoc] [-signcore] [-signorigin]\n\n" +
                "DESCRIPTION\n" +
                "  OfficeSigner demonstrates the usage of OfficeSigner from SecureBlackbox.\n" +
                "  Used to sign office documents.\n\n" +
                "  The options are as follows:\n\n" +
                "  -input        An input file to sign (Required).\n\n" +
                "  -output       Where the signed file will be saved (Required).\n\n" +
                "  -cert         The certificate used to sign files (Required).\n\n" +
                "  -certpass     The password for the signing certificate.\n\n" +
                "  -sigtype      The type of signature to use. Enter the corresponding number. Valid values:\n\n" +
                "                  0 - OST_DEFAULT\n" +
                "                  1 - OST_BINARY_CRYPTO_API\n" +
                "                  2 - OST_BINARY_XML\n" +
                "                  3 - OST_OPEN_XML\n" +
                "                  4 - OST_OPEN_XPS\n" +
                "                  5 - OST_OPEN_DOCUMENT\n\n" +
                "  -hashalg      The hash algorithm. Enter the corresponding string. Valid values: SHA1, SHA256, SHA384, SHA512, SHA224, MD5\n\n" +
                "  -signdoc      Whether to sign the document itself.\n\n" +
                "  -signcore     Whether to sign the core properties of the document.\n\n" +
                "  -signorigin   Whether to sign the signature origin.\n\n" +
                "EXAMPLES\n" +
                "  officesigner -input C:\\office\\helloworld.txt -output C:\\office\\myoffice.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n" +
                "  officesigner -input C:\\office\\helloworld.txt -output C:\\office\\myoffice.scs -cert C:\\certs\\mycert.pfx -certpass mypassword\n" +
                "             -sigtype 2 -hashalg SHA256 -signdoc -signcore -signorigin\n"
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
            OfficeSigner signer = new OfficeSigner();
            CertificateManager cm = new CertificateManager();

            if (optext(args, "-input"))
            {
                signer.InputFile = optval(args, "-input");
            }
            else
            {
                displayHelp("-input is required.");
                return;
            }

            if (optext(args, "-output"))
            {
                signer.OutputFile = optval(args, "-output");
            }
            else
            {
                displayHelp("-output is required.");
                return;
            }

            if (optext(args, "-cert"))
            {
                cm.ImportFromFile(optval(args, "-cert"), optval(args, "-certpass"));
                signer.SigningCertificate = cm.Certificate;
            }
            else
            {
                displayHelp("-cert is required.");
                return;
            }

            // Additional options
            if (optext(args, "-sigtype"))
            {
                signer.NewSignature.SignatureType = (OfficeSignatureTypes)int.Parse(optval(args, "-sigtype"));
            }
            if (optext(args, "-hashalg"))
            {
                signer.NewSignature.HashAlgorithm = optval(args, "-hashalg");
            }
            signer.NewSignature.DocumentSigned = optext(args, "-signdoc");
            signer.NewSignature.CorePropertiesSigned = optext(args, "-signcore");
            signer.NewSignature.SignatureOriginSigned = optext(args, "-signorigin");

            signer.Sign();

            Console.WriteLine("Office file successfully signed.\n");

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