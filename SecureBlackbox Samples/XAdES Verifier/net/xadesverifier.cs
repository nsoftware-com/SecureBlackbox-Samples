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
using SBMath;

class xadesverifier
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
                "  xadesverifier -- SecureBlackbox XAdESverifier Demo Application\n\n" +
                "SYNOPSIS\n" +
                "  xadesverifier <-input input_file> [-data original_data] [-cert certificate_file] [-certpass certificate_password] \n" +
                "                [-detached] [-showsigs] [-showrefs]\n" +
                "DESCRIPTION\n" +
                "  XAdESVerifier demonstrates the usage of XAdESVerifier from SecureBlackbox.\n" +
                "  Used to verify an XML Extended Signature (XAdES) from an XML file.\n\n" +
                "  The options are as follows:\n\n" +
                "  -input        A signature to verify (Required). If the signature is detached, this will take\n" +
                "                the signature file and -data will take the original data.\n\n" +
                "  -cert         The certificate used to verify the signature. Required if no key is included in the signature.\n\n" +
                "  -certpass     The password for the certificate.\n\n" +
                "  -detached     Whether the signature is detached. Use -data to specify the original data.\n\n" +
                "  -data         The original data.\n\n" +
                "  -showinfo     Whether to display detailed XAdES options used with the signature.\n\n" +
                "  -showrefs     Whether to display detailed results of reference verification.\n\n" +
                "EXAMPLES\n" +
                "  xadesverifier -input C:\\xades\\mysigned.xml\n" +
                "  xadesverifier -input C:\\xades\\mysigned.xml -detached -data C:\\xades\\my.xml\n" +
                "  xadesverifier -input C:\\xades\\mysigned.xml -cert C:\\certs\\mycert.pfx -certpass test\n" +
                "  xadesverifier -input C:\\xades\\mysigned.xml -showsigs -showrefs\n"
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

    private static String translateXAdESVersion(XAdESVersions ver)
    {
        switch (ver)
        {
            case XAdESVersions.xav111: return "1.1.1";
            case XAdESVersions.xav122: return "1.2.2";
            case XAdESVersions.xav132: return "1.3.2";
            case XAdESVersions.xav141: return "1.4.1";
            default: return "Unknown";
        }
    }

    private static String translateLevel(AdESSignatureLevels form)
    {
        switch (form)
        {
            case AdESSignatureLevels.aslGeneric: return "XML-DSIG";
            case AdESSignatureLevels.aslBaselineB: return "XAdES Baseline B";
            case AdESSignatureLevels.aslBaselineT: return "XAdES Baseline T";
            case AdESSignatureLevels.aslBaselineLT: return "XAdES Baseline LT";
            case AdESSignatureLevels.aslBaselineLTA: return "XAdES Baseline LTA";
            case AdESSignatureLevels.aslBES: return "XAdES-BES";
            case AdESSignatureLevels.aslEPES: return "XAdES-EPES";
            case AdESSignatureLevels.aslT: return "XAdES-T";
            case AdESSignatureLevels.aslC: return "XAdES-C";
            case AdESSignatureLevels.aslX: return "XAdES-X";
            case AdESSignatureLevels.aslXL: return "XAdES-X-L";
            case AdESSignatureLevels.aslA: return "XAdES-A";
            case AdESSignatureLevels.aslExtendedBES: return "XAdES-E-BES";
            case AdESSignatureLevels.aslExtendedEPES: return "XAdES-E-EPES";
            case AdESSignatureLevels.aslExtendedT: return "XAdES-E-T";
            case AdESSignatureLevels.aslExtendedC: return "XAdES-E-C";
            case AdESSignatureLevels.aslExtendedX: return "XAdES-E-X";
            case AdESSignatureLevels.aslExtendedXLong: return "XAdES-E-X-Long";
            case AdESSignatureLevels.aslExtendedXL: return "XAdES-E-X-L";
            case AdESSignatureLevels.aslExtendedA: return "XAdES-E-A";
            default: return "Unknown";
        }
    }

    private static String translateValidationResult(SignatureValidities res)
    {
        switch (res)
        {
            case SignatureValidities.svtValid:
                return "The signature is valid.";
            case SignatureValidities.svtCorrupted:
                return "The signature is corrupted.";
            case SignatureValidities.svtSignerNotFound:
                return "Failed to acquire the signing certificate. The signature cannot be validated.";
            case SignatureValidities.svtFailure:
                return "General failure.";
            case SignatureValidities.svtReferenceCorrupted:
                return "The signature has invalid reference(s).";
            default:
                return "Signature validity is unknown.";
        }
    }

    private static bool showrefs = false;

    private static XAdESVerifier verifier;

    private static void _verifier_OnReferenceValidated(object sender, XAdESVerifierReferenceValidatedEventArgs e)
    {
        if (showrefs)
        {
            String valid = "false";
            if (e.DigestValid)
                valid = "true";
            Console.WriteLine(e.ID + "	" + e.URI + "	" + e.RefType + "	" + valid);
        }
    }

    static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            displayHelp("");
            return;
        }

        XAdESVerifier verifier = new XAdESVerifier();
        try
        {
            verifier.OnReferenceValidated += _verifier_OnReferenceValidated;

            if (optext(args, "-input"))
            {
                verifier.InputFile = optval(args, "-input");
            }
            else
            {
                displayHelp("-input is required.");
                return;
            }

            bool detached = false;
            if (optext(args, "-detached"))
            {
                detached = true;

                if (optext(args, "-data"))
                {
                    verifier.DataFile = optval(args, "-data");
                    verifier.DataType = XAdESVerifierDataTypes.cxdtBinary;
                    verifier.DataURI = "filename.txt"; // use real name of the input
                }
                else
                {
                    displayHelp("-data is required if -detached is used.");
                    return;
                }
            }

            if (optext(args, "-cert"))
            {
                CertificateManager cm = new CertificateManager();
                cm.ImportFromFile(optval(args, "-cert"), optval(args, "-certpass"));
                verifier.KnownCertificates.Add(cm.Certificate);
            }

            if (optext(args, "-showrefs"))
            {
                showrefs = true;
                Console.WriteLine(
                        "ID URI RefType DigestValid?\n" +
                        "---------------------"
                );
            }

            if (detached)
            {
                verifier.VerifyDetached();
            }
            else
            {
                verifier.Verify();
            }

            Console.WriteLine("\nVerification complete.\n");
            for (int i = 0; i < verifier.Signatures.Count; i++)
            {
                Console.WriteLine("Signature #" + (i + 1));
                Console.WriteLine("  Validation Result: " + verifier.Signatures[i].SignatureValidationResult + ", " + translateValidationResult(verifier.Signatures[i].SignatureValidationResult));
                Console.WriteLine("  Chain Result: " + verifier.Signatures[i].ChainValidationResult + "\n");

                if (optext(args, "-showinfo"))
                {
                    Console.WriteLine("XAdES Detailed Information:");
                    Console.WriteLine("   Version: " + translateXAdESVersion(verifier.Signatures[i].XAdESVersion));
                    Console.WriteLine("   Level/Form: " + translateLevel(verifier.Signatures[i].Level));
                }
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