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

class simplepdfsigner
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
                "  simplepdfsigner -- SecureBlackbox PDFSigner Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  simplepdfsigner <-input input_file> <-output output_file> [-cert certificate_file] [-certpass certificate_password]\n" +
                "             [-pkcs11 pkcs11_file] [-pin pkcs11_pin] [-win32 win32_name]\n\n" +
                "DESCRIPTION\n" +
                "  PDFSigner demonstrates the usage of PDFSigner from SecureBlackbox.\n" +
                "  This sample illustrates the use of PDFSigner component for signing PDF documents. \n\n" +
                "  The options are as follows:\n\n" +
                "  -input        An input file to sign (Required).\n\n" +
                "  -output       Where the signed file will be saved (Required).\n\n" +
                "  -cert         The certificate used to sign files.\n\n" +
                "  -certpass     The password for the signing certificate.\n\n" +
                "  -pkcs11       The pkcs11 storage used to sign file.\n\n" +
                "  -pin          The PIN for pkcs11 storage\n\n" +
                "  -win32        The win32 store name\n\n" +
                "EXAMPLES\n" +
                "  simplepdfsigner -input C:\\pdf\\myfile.pdf -output C:\\pdf\\mysignedfile.pdf -cert C:\\certs\\mycert.pfx -certpass mypassword \n\n" +
                "  simplepdfsigner -input C:\\pdf\\myfile.pdf -output C:\\pdf\\mysignedfile.pdf -pkcs11 C:\\pkcs11\\pkcs11.dll -pin mypassword \n\n" +
                "  simplepdfsigner -input C:\\pdf\\myfile.pdf -output C:\\pdf\\mysignedfile.pdf -win32 My \n"
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

        PDFSigner signer = new PDFSigner();
        CertificateStorage certstorage = new CertificateStorage();
        try
        {
            String certFile = "";
            String certPass = "";
            String pkcs11File = "";
            String pin = "";
            String win32Store = "My";

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
                certFile = optval(args, "-cert");
            }

            if (optext(args, "-certpass"))
            {
                certPass = optval(args, "-certpass");
            }

            if (optext(args, "-pkcs11"))
            {
                pkcs11File = optval(args, "-pkcs11");
            }

            if (optext(args, "-pin"))
            {
                pin = optval(args, "-pin");
            }

            if (optext(args, "-win32"))
            {
                win32Store = optval(args, "-win32");
            }

            if ((certFile.Length == 0) && (pkcs11File.Length == 0) && (win32Store.Length == 0))
            {
                displayHelp("-cert or -pkcs11 or -win32 is required.");
                return;
            }

            if ((certFile.Length > 0) && (pkcs11File.Length > 0) && (win32Store.Length > 0))
            {
                displayHelp("Use only one -cert or -pkcs11 or -win32 parameter.");
                return;
            }

            if (certFile.Length > 0)
            {
                CertificateManager cm = new CertificateManager();
                cm.ImportFromFile(certFile, certPass);
                signer.SigningCertificate = cm.Certificate;
            }
            else
            {
                try
                {
                    if (pkcs11File.Length > 0)
                    {
                        certstorage.Open("pkcs11://user:" + pin + "@/" + pkcs11File);
                    }
                    else
                    {
                        certstorage.Open("system://?store=" + win32Store);
                    }

                    signer.SigningCertificate = certstorage.Certificates[0];
                }
                catch (Exception ex)
                {
                    Console.WriteLine("Error: Cannot load certificate!");
                    return;
                }
            }

            signer.NewSignature.Level = PAdESSignatureLevels.paslBES;
            signer.Widget.Invisible = false;
            signer.IgnoreChainValidationErrors = true;

            signer.Sign();

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