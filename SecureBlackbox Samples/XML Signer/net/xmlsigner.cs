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

class xmlsigner
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
                "  xmlsigner -- SecureBlackbox XMLSigner Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  xmlsigner <-input input_file> <-output output_file> <-cert certificate_file> [-certpass certificate_password] \n" +
                "           [-detached] [-canonmethod canon_method] [-hashalg hash_algorithm] [-includekey] \n\n" +
                "DESCRIPTION\n" +
                "  XMLSigner demonstrates the usage of XMLSigner from SecureBlackbox.\n" +
                "  Used to create an XML Signature from an XML file.\n\n" +
                "  The options are as follows:\n\n" +
                "  -input        An input file to sign (Required).\n\n" +
                "  -output       Where the XML signature will be saved (Required).\n\n" +
                "  -cert         The certificate used to sign files (Required).\n\n" +
                "  -certpass     The password for the signing certificate.\n\n" +
                "  -detached     Whether the signature is detached.\n\n" +
                "  -canonmethod  The canonicalization method to use. Enter the corresponding number. Valid values:\n\n" +
                "                  0 - CXCM_NONE\n" +
                "                  1 - CXCM_CANON\n" +
                "                  2 - CXCM_CANON_COMMENT\n" +
                "                  3 - CXCM_EXCL_CANON\n" +
                "                  4 - CXCM_EXCL_CANON_COMMENT\n" +
                "                  5 - CXCM_MIN_CANON\n" +
                "                  6 - CXCM_CANON_V_1_1\n" +
                "                  7 - CXCM_CANON_COMMENT_V_1_1\n\n" +
                "  -hashalg      The hashing algorithm to use. Valid values:\n\n" +
                "                  SHA1\n" +
                "                  MD5\n" +
                "                  SHA256\n" +
                "                  SHA384\n" +
                "                  SHA512\n" +
                "                  RIPEMD160\n\n" +
                "  -includekey   Whether to include the public key in the signature.\n\n" +
                "EXAMPLES\n" +
                "  xmlsigner -input C:\\xml\\myfile.xml -output C:\\xml\\mysignedfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword\n" +
                "  xmlsigner -input C:\\xml\\myfile.xml -output C:\\xml\\mysignedfile.xml -cert C:\\certs\\mycert.pfx -certpass mypassword\n" +
                "           -canonmethod 3 -hashalg SHA1 -detached\n"
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

        XMLSigner signer = new XMLSigner();
        CertificateManager cm = new CertificateManager();
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
            if (optext(args, "-detached"))
            {
                signer.DataFile = input;
                signer.DataType = XMLSignerDataTypes.cxdtBinary;
                signer.DataURI = "filename.txt"; // use real name of the input
                signer.SignatureType = XMLSignerSignatureTypes.cxstDetached;
            }
            else
            {
                signer.InputFile = input;
                signer.SignatureType = XMLSignerSignatureTypes.cxstEnveloped;
            }

            signer.CanonicalizationMethod = (XMLSignerCanonicalizationMethods)int.Parse(optval(args, "-canonmethod"));

            if (optext(args, "-hashalg"))
            {
                signer.HashAlgorithm = optval(args, "-hashalg");
            }
            else
            {
                signer.HashAlgorithm = "SHA256";
            }

            if (optext(args, "-includekey"))
            {
                signer.Config("IncludeKey=true");
            }

            // Sign
            signer.Sign();

            Console.WriteLine("XML file successfully signed.\n");

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