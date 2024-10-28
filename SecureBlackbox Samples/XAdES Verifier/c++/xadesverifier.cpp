/*
 * SecureBlackbox 2024 C++ Edition - Sample Project
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
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../../include/secureblackbox.h"

class MyVerifier : public XAdESVerifier {
public:

    bool showrefs = false;

    int FireReferenceValidated(XAdESVerifierReferenceValidatedEventParams* e) override {
        if (showrefs) {
            printf(
                "%.10s %.22s %.22s %s\n",
                e->ID, e->URI, e->RefType, e->DigestValid ? "true" : "false"
            );
        }
        return 0;
    }
};

namespace ArgParser {
    static char* optval(int argc, char** argv, const char* option) {
        for (int x = 0; x < argc - 1; x++) {
            if (!strcmp(argv[x], option)) {
                return argv[x + 1];
            }
        }
        return (char*)"";
    }

    static bool optext(int argc, char** argv, const char* option) {
        for (int x = 0; x < argc; x++) {
            if (!strcmp(argv[x], option)) {
                return true;
            }
        }
        return false;
    }
};

using namespace ArgParser;

void displayHelp(const char* errMes) {
    printf(
        "NAME\n"
        "  xadesverifier -- SecureBlackbox XAdESverifier Demo Application\n\n"
        "SYNOPSIS\n"
		"  xadesverifier <-input input_file> [-data original_data] [-cert certificate_file] [-certpass certificate_password] \n"
        "                [-detached] [-showsigs] [-showrefs]\n"
        "DESCRIPTION\n"
        "  XAdESVerifier demonstrates the usage of XAdESVerifier from SecureBlackbox.\n"
        "  Used to verify an XML Extended Signature (XAdES) from an XML file.\n\n"
        "  The options are as follows:\n\n"
        "  -input        A signature to verify (Required). If the signature is detached, this will take\n"
        "                the signature file and -data will take the original data.\n\n"
        "  -cert         The certificate used to verify the signature. Required if no key is included in the signature.\n\n"
        "  -certpass     The password for the certificate.\n\n"
        "  -detached     Whether the signature is detached. Use -data to specify the original data.\n\n"
        "  -data         The original data.\n\n"
        "  -showinfo     Whether to display detailed XAdES options used with the signature.\n\n"
        "  -showrefs     Whether to display detailed results of reference verification.\n\n"
        "EXAMPLES\n"
        "  xadesverifier -input C:\\xades\\mysigned.xml\n"
        "  xadesverifier -input C:\\xades\\mysigned.xml -detached -data C:\\xades\\my.xml\n"
        "  xadesverifier -input C:\\xades\\mysigned.xml -cert C:\\certs\\mycert.pfx -certpass test\n"
        "  xadesverifier -input C:\\xades\\mysigned.xml -showsigs -showrefs\n\n"
    );

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

const char* translateXAdESVersion(int ver) {
    switch (ver) {
    case XAV_111: return "1.1.1";
        break;
    case XAV_122: return "1.2.2";
        break;
    case XAV_132: return "1.3.2";
        break;
    case XAV_141: return "1.4.1";
        break;
    default: return "Unknown";
        break;
    }
}

const char* translateLevel(int level) {
    switch (level) {
    case ASL_GENERIC: return "XML-DSIG";
        break;
    case ASL_BES: return "XAdES-BES";
        break;
    case ASL_EPES: return "XAdES-EPES";
        break;
    case ASL_T: return "XAdES-T";
        break;
    case ASL_C: return "XAdES-C";
        break;
    case ASL_X: return "XAdES-X";
        break;
    case ASL_XL: return "XAdES-X-L";
        break;
    case ASL_A: return "XAdES-A";
        break;
    case ASL_EXTENDED_BES: return "XAdES-E-BES";
        break;
    case ASL_EXTENDED_EPES: return "XAdES-E-EPES";
        break;
    case ASL_EXTENDED_T: return "XAdES-E-T";
        break;
    case ASL_EXTENDED_C: return "XAdES-E-C";
        break;
    case ASL_EXTENDED_X: return "XAdES-E-X";
        break;
    case ASL_EXTENDED_XLONG: return "XAdES-E-X-Long";
        break;
    case ASL_EXTENDED_XL: return "XAdES-E-X-L";
        break;
    case ASL_EXTENDED_A: return "XAdES-E-A";
        break;
    default: return "Unknown";
        break;
    }
}

const char* translateValidationResult(int res) {
  switch (res) {
  case SVT_VALID:
    return "The signature is valid.";
    break;
  case SVT_UNKNOWN:
    return "Signature validity is unknown.";
    break;
  case SVT_CORRUPTED:
    return "The signature is corrupted.";
    break;
  case SVT_SIGNER_NOT_FOUND:
    return "Failed to acquire the signing certificate. The signature cannot be validated.";
    break;
  case SVT_FAILURE:
    return "General failure.";
    break;
  case SVT_REFERENCE_CORRUPTED:
    return "The signature has invalid reference(s).";
    break;
  default:
    return "Signature validity is unknown.";
    break;
  }
}

int main(int argc, char** argv) {
    MyVerifier verifier;
    CertificateManager cm;

    if (argc < 2) {
        displayHelp("");
        goto done;
    }

    char* input = optval(argc, argv, "-input");
    if (!strcmp(input, "")) {
        displayHelp("-input is required.");
        goto done;
    }
    verifier.SetInputFile(input);

    bool detached = false;
    if (optext(argc, argv, "-detached")) {
        detached = true;
        char* datafile = optval(argc, argv, "-data");
        if (strcmp(datafile, "")) {
          verifier.SetDataFile(datafile);
          verifier.SetDataType(CXDT_BINARY);
          verifier.SetDataURI("filename.txt"); // use real name of the input
        } else {
			displayHelp("-data is required if -detached is used.");
            goto done;
        }
    }

    if (optext(argc, argv, "-cert")) {
        cm.ImportFromFile(optval(argc, argv, "-cert"), optval(argc, argv, "-certpass"));
        verifier.SetKnownCertCount(1);
        verifier.SetKnownCertHandle(0, cm.GetCertHandle());
    }

    if (optext(argc, argv, "-showrefs")) {
      verifier.showrefs = true;
        printf(
            "ID URI RefType DigestValid?\n"
            "---------------------\n"
        );
    }

    if (detached)
    {
      if (verifier.VerifyDetached()) {
        goto err;
      }
    }
    else
    {
      if (verifier.Verify()) {
        goto err;
      }
    }

    printf("\nVerification complete.\n\n");
    for (int i = 0; i < verifier.GetSignatureCount(); i++)
    {
      printf("Signature #%i\n", i);

      printf("  Validation Result: %d, %s\n", verifier.GetSignatureSignatureValidationResult(i), translateValidationResult(verifier.GetSignatureSignatureValidationResult(i)));
      printf("  Chain Result: %d\n\n", verifier.GetSignatureChainValidationResult(i));

      if (optext(argc, argv, "-showinfo")) {
        printf(
          "XAdES Detailed Information:\n"
          "XAdES Version: %s\n"
          "   Level/XAdES form: %s\n\n",
          translateXAdESVersion(verifier.GetSignatureXAdESVersion(i)),
          translateLevel(verifier.GetSignatureLevel(i)));
      }
    }

err:
    if (verifier.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", verifier.GetLastErrorCode(), verifier.GetLastError());
    }
done:
	printf("Press Enter to exit the demo.\n");
    getchar();
}


