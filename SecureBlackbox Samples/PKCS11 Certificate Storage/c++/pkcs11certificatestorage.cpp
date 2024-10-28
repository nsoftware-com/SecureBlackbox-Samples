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
#include <sstream>
#include <string>
#include <iostream>
#include <vector>
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
};

using namespace std;
using namespace ArgParser;

void displayHelp(const char* errMes) {
    printf(
        "NAME\n"
        "  pkcs11certificatestorage -- SecureBlackbox CertificateStorage Demo Application\n\n"
        "SYNOPSIS\n"
        "  pkcs11certificatestorage <-storage driver_path> [-pin pin] [-slot slot_num]\n\n"
        "DESCRIPTION\n"
        "  This sample illustrates the use of CertificateStorage component to access HSMs via PKCS11 interface. \n\n"
        "  The options are as follows:\n\n"
        "  -storage      A path to the pkcs11 driver file (Required).\n\n"
        "  -pin          The user PIN for the device. If no PIN is provided, the sample won't be signing in.\n\n"
        "  -slot         The slot number to use. If not specified, the first slot with a token in will be used. Pass -1 as a slot number to only list the slots.\n\n"
        "EXAMPLES\n"
        "  pkcs11certificatestorage -storage C:\\pkcs11\\pkcs11.dll -pin mypassword\n\n"
    );

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char** argv) {
    CertificateStorage cs;
	std::stringstream ss;
	auto slotList = std::vector<std::string>{};

    char storeuri[100];
    char confrequest[100];
    char parPin[32];
    char parSlot[32];
    char parLogin[32];

    int certidx = -1;

    // Validate input
    if (argc < 2) {
        displayHelp("");
        goto done;
    }

    char* parDrvPath = optval(argc, argv, "-storage");
    if (!strcmp(parDrvPath, "")) {
        displayHelp("-storage is required.");
        goto done;
    }

    char* pin = optval(argc, argv, "-pin");
    char* slot = optval(argc, argv, "-slot");

    // if slot is not provided, the first slot with the token in will be opened
    sprintf(parSlot, (strlen(slot) > 0) ? "&slot=%s" : "%s", slot);

    // if pin is provided, signing in with it
    sprintf(parPin, (strlen(pin) > 0) ? ":%s" : "%s", pin);
    sprintf(parLogin, "login=%s", (strlen(pin) > 0) ? "yes" : "no");

    // forming the storage URI
    sprintf(storeuri, "pkcs11://user%s@localhost/%s?%s%s", parPin, parDrvPath, parLogin, parSlot);

    // Opening the storage
    printf("Opening storage: %s\n\n", storeuri);
    if (cs.Open(storeuri)) {
        goto done;
    }

    printf("The storage has been opened. Browsing for slots...\n\n");

    // Listing slots
	ss << cs.ListStores();
	for (std::string line; std::getline(ss, line, '\n');)
		slotList.push_back(line);
	
	printf("%d slot(s) have been found\n\n", slotList.size());

	for (int x = 0; x < slotList.size(); x++) {
		sprintf(confrequest, "PKCS11SlotTokenPresent[%d]", x);
		const int tokenFound = !strcmp(cs.Config(confrequest), "True");

		if (tokenFound) {
			cout << "- Slot: " << (x + 1) << " (with token): " << slotList.at(x) << endl << endl;
		} 
		else {
			cout << "- Slot: " << (x + 1) << " (no token): " << slotList.at(x) << endl << endl;
		}
	}

    // Listing certificates
    printf("%d certificates(s) have been found (can be zero if an empty slot was selected)\n\n", cs.GetCertCount());

    for (int y = 0; y < cs.GetCertCount(); y++) {
        const int keyFound = cs.GetCertPrivateKeyExists(y);
        if ((keyFound) && (certidx < 0)) {
            certidx = y;
        }
        printf("- Cert %d: %s (%s)\n\n", y, cs.GetCertSubjectRDN(y), keyFound ? "private key found" : "no private key");
    }

    // Closing the storage
    cs.Close(false);

err:
    if (cs.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", cs.GetLastErrorCode(), cs.GetLastError());
    }
done:
	printf("Press Enter to exit the demo.\n");
    getchar();
}


