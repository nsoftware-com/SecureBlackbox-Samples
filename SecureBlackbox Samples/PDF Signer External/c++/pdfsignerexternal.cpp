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

char* keyFile;
CryptoKeyManager keymanager;
PublicKeyCrypto crypto;

unsigned char* hexstr_to_char(const char* hexstr) {
    const size_t len = strlen(hexstr);
    const size_t final_len = len / 2;
    unsigned char* chrs = (unsigned char*)malloc((final_len + 1) * sizeof(*chrs));
    for (size_t i = 0, j = 0; j < final_len; i += 2, j++) {
        chrs[j] = (hexstr[i] % 32 + 9) % 25 * 16 + (hexstr[i + 1] % 32 +
            9) % 25;
    }
    chrs[final_len] = '\0';
    return chrs;
}

char* barray2hexstr(const unsigned char* data, size_t datalen) {
    const size_t final_len = datalen * 2;
    char* chrs = (char*)malloc((final_len + 1) * sizeof(*chrs));
    unsigned int j = 0;
    for (j = 0; j < datalen; j++) {
        chrs[2 * j] = (data[j] >> 4) + 48;
        chrs[2 * j + 1] = (data[j] & 15) + 48;
        if (chrs[2 * j] > 57) {
            chrs[2 * j] += 7;
        }
        if (chrs[2 * j + 1] > 57) {
            chrs[2 * j + 1] += 7;
        }
    }
    chrs[2 * j] = '\0';
    return chrs;
}

class MyPDFSigner : public PDFSigner {
public:

    int FireExternalSign(PDFSignerExternalSignEventParams* e) override {
        keymanager.ImportFromFile(keyFile, 3, "", "", e->Pars, 0, "");

        if (keymanager.GetLastErrorCode() == 0) {
            crypto.SetKeyHandle(keymanager.GetKeyHandle());
        } else {
            printf("Error [%d]: %s", keymanager.GetLastErrorCode(), keymanager.GetLastError());
            return 0;
        }

        crypto.SetHashAlgorithm(e->HashAlgorithm);
        crypto.SetInputIsHash(true);
        crypto.SetSchemeParams(e->Pars);

        char* inBuf = (char*)hexstr_to_char(e->Data);

        int outSize;
        char* outBuf = crypto.Sign(inBuf, strlen(inBuf), true, &outSize);

        e->SignedData = barray2hexstr((unsigned char*)outBuf, outSize);

        return 0;
    }
};

void displayHelp(const char* errMes) {
	printf(
		"NAME\n"
		"  pdfsignerexternal -- SecureBlackbox PDFSigner Demo Application\n\n"
		"SYNOPSIS\n"
		"  pdfsignerexternal <-input input_file> <-output output_file> <-key key_file> <-cert certificate_file> \n"
		"                [-certpass certificate_password] [-sigtype sig_type] [-level sig_level]\n\n"
		"DESCRIPTION\n"
		"  This sample illustrates the use of PDFSigner component for signing PDF documents.\n\n"
		"  The options are as follows:\n\n"
		"  -input        An input file to sign (Required).\n\n"
		"  -output       Where the signed file will be saved (Required).\n\n"
		"  -key          The key file to be imported (Required).\n\n"
		"  -cert         The certificate used to sign files (Required).\n\n"
		"  -certpass     The password for the signing certificate.\n\n"
		"  -sigtype      The signature type. Enter the corresponding number. Valid values:\n\n"
		"                  0  - PST_UNKNOWN\n"
		"                  1  - PST_LEGACY\n"
		"                  2  - PST_PADES\n"
		"                  3  - PST_DOCUMENT_TIMESTAMP\n"
		"  -level        The level for PAdES signatures. Enter the corresponding number. Valid values:\n\n"
		"                  0  - PASL_UNKNOWN\n"
		"                  1  - PASL_GENERIC\n"
		"                  2  - PASL_BASELINE_B\n"
		"                  3  - PASL_BASELINE_T\n"
		"                  4  - PASL_BASELINE_LT\n"
		"                  5  - PASL_BASELINE_LTA\n"
		"                  6  - PASL_BES\n"
		"                  7  - PASL_EPES\n"
		"                  8  - PASL_LTV\n\n"
		"EXAMPLES\n"
		"  pdfsignerexternal -input C:\\helloworld.pdf -output C:\\sign.pdf -key C:\\certs\\mykey.key -cert C:\\certs\\mycert.pfx -certpass mypassword\n\n"
	);

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char** argv) {
	MyPDFSigner signer;
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

	char* output = optval(argc, argv, "-output");
	if (!strcmp(output, "")) {
		displayHelp("-output is required.");
		goto done;
	}

	keyFile = optval(argc, argv, "-key");
	if (!strcmp(keyFile, "")) {
		displayHelp("-key is required.");
		goto done;
	}

	char* cert = optval(argc, argv, "-cert");
	if (!strcmp(cert, "")) {
		displayHelp("-cert is required.");
		goto done;
	}

	char* certpass = optval(argc, argv, "-certpass");

	// Required options
	signer.SetInputFile(input);
	signer.SetOutputFile(output);
	cm.ImportFromFile(cert, certpass);
	signer.SetSigningCertHandle(cm.GetCertHandle());
	if (cm.GetCertKeyAlgorithm() == "id-dsa") {
		signer.SetNewSigHashAlgorithm("SHA1");
	}

	if (optext(argc, argv, "-sigtype")) {
		signer.SetNewSigSignatureType(atoi(optval(argc, argv, "-sigtype")));
	}
	if (optext(argc, argv, "-level")) {
		signer.SetNewSigLevel(atoi(optval(argc, argv, "-level")));
	}

    signer.SetNewSigAuthorName("test demo author");
    signer.SetNewSigReason("test demo reason");

	signer.SetIgnoreChainValidationErrors(TRUE);
	signer.SetExternalCryptoMode(ECM_GENERIC);

    if (signer.SignExternal()) {
        goto err;
    } else {
        printf("The document was signed successfully.\n\n");
    };

err:
	if (signer.GetLastErrorCode()) {
		printf("Error: [%i] %s\n", signer.GetLastErrorCode(), signer.GetLastError());
	}
done:
	printf("Press Enter to exit the demo.\n");
	getchar();
}


