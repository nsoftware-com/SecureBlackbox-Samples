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
using System.Linq;
using System.Text;
using nsoftware.SecureBlackbox;
using SBMath;

class pdfsignerexternal
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
                "  pdfsignerexternal -- SecureBlackbox PDFSigner Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  pdfsignerexternal <-input input_file> <-output output_file> <-key key_file> <-cert certificate_file>\n" +
                "                 [-certpass certificate_password] [-level sig_level]\n\n" +
                "DESCRIPTION\n" +
                "  This sample illustrates the use of PDFSigner component for signing PDF documents.\n\n" +
                "  The options are as follows:\n\n" +
                "  -input        An input file to sign (Required).\n\n" +
                "  -output       Where the signed file will be saved (Required).\n\n" +
                "  -key          The key file to be imported (Required).\n\n" +
                "  -cert         The certificate used to sign files (Required).\n\n" +
                "  -certpass     The password for the signing certificate.\n\n" +
                "  -level        The level for PAdES signatures. Enter the corresponding number. Valid values:\n\n" +
                "                  0  - PASL_UNKNOWN\n" +
                "                  1  - PASL_GENERIC\n" +
                "                  2  - PASL_BASELINE_B\n" +
                "                  3  - PASL_BASELINE_T\n" +
                "                  4  - PASL_BASELINE_LT\n" +
                "                  5  - PASL_BASELINE_LTA\n" +
                "                  6  - PASL_BES\n" +
                "                  7  - PASL_EPES\n" +
                "                  8  - PASL_LTV\n\n" +
                "EXAMPLES\n" +
                "  pdfsignerexternal -input C:\\helloworld.pdf -output C:\\sign.pdf -key test.key -cert C:\\certs\\mycert.pfx -certpass mypassword\n"
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

    private static byte[] HexStringToByteArray(string hex)
    {
        return Enumerable.Range(0, hex.Length)
                         .Where(x => x % 2 == 0)
                         .Select(x => Convert.ToByte(hex.Substring(x, 2), 16))
                         .ToArray();
    }

    private static string ByteArrayToHexString(byte[] ba)
    {
        StringBuilder hex = new StringBuilder(ba.Length * 2);
        foreach (byte b in ba)
            hex.AppendFormat("{0:x2}", b);
        return hex.ToString();
    }

    private static string keyFile = "";
    private static PDFSigner signer;
    private static void _signer_OnExternalSign(object sender, PDFSignerExternalSignEventArgs e)
    {
        PublicKeyCrypto crypto = new PublicKeyCrypto();
        try
        {
            CryptoKeyManager keymanager = new CryptoKeyManager();

            keymanager.ImportFromFile(keyFile, 3, "", "", e.Pars, 0, "");

            crypto.Key = keymanager.Key;
        }
        catch (Exception ex)
        {
            Console.WriteLine("Cannot load key!");
        }

        try
        {
            crypto.HashAlgorithm = e.HashAlgorithm;
            crypto.InputIsHash = true;
            crypto.SchemeParams = e.Pars;

            byte[] inBuf = HexStringToByteArray(e.Data);
            byte[] outBuf = crypto.Sign(inBuf, true);

            e.SignedData = ByteArrayToHexString(outBuf);
        }
        catch (Exception ex)
        {
            Console.WriteLine("Cannot signing data!");
        }
    }

    static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            displayHelp("");
            return;
        }

        signer = new PDFSigner();
        CertificateManager cm = new CertificateManager();
        try
        {
            signer.OnExternalSign += _signer_OnExternalSign;


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

            if (optext(args, "-key"))
            {
                keyFile = optval(args, "-key");
            }
            else
            {
                displayHelp("-key is required.");
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

            signer.NewSignature.AuthorName = "test demo author";
            signer.NewSignature.Reason = "test demo reason";
            signer.Widget.Invisible = false;

            signer.IgnoreChainValidationErrors = true;
            signer.ExternalCrypto.Mode = ExternalCryptoModes.ecmGeneric;

            signer.SignExternal();

            Console.WriteLine("PDF file successfully signed.\n");

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