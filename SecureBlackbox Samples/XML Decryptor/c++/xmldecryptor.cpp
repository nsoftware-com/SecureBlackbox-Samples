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
#include <string.h>
#include "../../include/secureblackbox.h"

class MyDecryptor : public XMLDecryptor {
public:
    char *cert, *certpass, *keypass;
    bool showrefs = false;

    static int getKeyLen(char* algorithm) {
int len = 0;

    if (!strcmp(algorithm, "AES128") || !strcmp(algorithm, "RC4") || !strcmp(algorithm, "Camellia128") || !strcmp(algorithm, "SEED")) {
        len = 16;
    } else if (!strcmp(algorithm, "AES192") || !strcmp(algorithm, "Camellia192") || !strcmp(algorithm, "3DES")) {
        len = 24;
    } else if (!strcmp(algorithm, "AES256") || !strcmp(algorithm, "Camellia256")) {
        len = 32;
    } else if (!strcmp(algorithm, "DES")) {
        len = 8;
    }
    return len;
}

    static char* getKey(char* algorithm, char* pass) {
        const int len = getKeyLen(algorithm);

        // simple key derivation function from a Passphrase
        // TODO: replace with SHA256 hash or KDF
        char* res = new char[len];
        strcpy(res, pass);
        while (strlen(res) < len) {
            strcat(res, "/");
            strcat(res, pass);
        }

        return res;
    }

    int FireDecryptionInfoNeeded(XMLDecryptorDecryptionInfoNeededEventParams* e) override {
        char* mes = new char[100];
        strcpy(mes, "Encryption method: ");
        strcat(mes, this->GetEncryptionMethod());
        if (this->GetUseGCM()) {
            strcat(mes, "-GCM");
        }
        printf(mes);
        printf("\n");

        if (this->GetEncryptedDataType() == 0) {
            printf("Encrypted data type: Element\n");
        } else if (this->GetEncryptedDataType() == 1) {
            printf("Encrypted data type: Content\n");
        } else {
            printf("Encrypted data type: External\n");
        }

        if (this->GetEncryptKey()) {
            printf("EncryptKey: true\n");
            if (!this->GetKeyEncryptionType()) {
                printf("Key encryption type: transport\n");
                printf(!this->GetKeyTransportMethod() ?
                           "Key transport method: RSA v1.5\n" :
                           "Key transport method: RSA-OAEP\n");
            } else {
                printf("Key encryption type: wrap\n");
                strcpy(mes, "Key wrap method: ");
                strcat(mes, this->GetKeyWrapMethod());
                printf(mes);
                printf("\n");
            }
        } else {
            printf("EncryptKey: false\n");
        }
        printf("\n");

        if (this->GetEncryptKey()) {
            if (this->GetKeyEncryptionType() == 0) {
                CertificateManager cm;
                cm.ImportFromFile(cert, certpass);
                this->SetKeyDecryptionCertHandle(cm.GetCertHandle());
            } else {
                this->SetKeyDecryptionKey(getKey(this->GetKeyWrapMethod(), keypass), getKeyLen(this->GetKeyWrapMethod()));
            }
        } else {
            this->SetDecryptionKey(getKey(this->GetEncryptionMethod(), keypass), getKeyLen(this->GetEncryptionMethod()));
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
};

using namespace ArgParser;

void displayHelp(const char* errMes) {
    printf(
        "NAME\n"
        "  xmldecryptor -- SecureBlackbox XMLDecryptor Demo Application\n\n"
        "SYNOPSIS\n"
		"  xmldecryptor <-input input_file> <-output output_file> [-cert certificate_file] [-certpass certificate_password] [-pass key_password] \n\n"
        "DESCRIPTION\n"
        "  XMLDecryptor demonstrates the usage of XMLDecryptor from SecureBlackbox.\n"
        "  Used to decrypt XML file.\n\n"
        "  The options are as follows:\n\n"
        "  -input        An input file to decrypt (Required). \n\n"
        "  -output       Where the decrypted XML file will be saved (Required). \n\n"
        "  -cert         The certificate used to decrypt file. \n\n"
        "  -certpass     The password for the certificate. \n\n"
        "  -pass         The password for the decrypting. \n\n"
        "EXAMPLES\n"
        "  xmldecryptor -input C:\\xml\\myenc.xml -output C:\\xml\\myfile.xml -pass mtpassword \n"
        "  xmldecryptor -input C:\\xml\\myenc.xml -output C:\\xml\\myfile.xml -cert C:\\certs\\mycert.pfx -certpass test -external C:\\xml\\external.xml \n\n"
    );

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char** argv) {
    MyDecryptor decryptor;

    if (argc < 2) {
        displayHelp("");
        goto done;
    }

    char* input = optval(argc, argv, "-input");
    if (!strcmp(input, "")) {
        displayHelp("-input is required.");
        goto done;
    }
    decryptor.SetInputFile(input);

    char* output = optval(argc, argv, "-output");
    if (!strcmp(output, "")) {
        displayHelp("-output is required.");
        goto done;
    }
    decryptor.SetOutputFile(output);

    decryptor.cert = optval(argc, argv, "-cert");
    decryptor.certpass = optval(argc, argv, "-certpass");
    decryptor.keypass = optval(argc, argv, "-pass");

    if (decryptor.Decrypt()) {
        goto err;
    }
    printf("XML file successfully decrypted.\n\n");

err:
    if (decryptor.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", decryptor.GetLastErrorCode(), decryptor.GetLastError());
    }
done:
	printf("Press Enter to exit the demo.\n");
    getchar();
}


