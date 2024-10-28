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

void displayHelp(const char* errMes) {
    printf(
        "NAME\n"
        "  passwordvault -- SecureBlackbox PasswordVault Demo Application\n\n"
        "SYNOPSIS\n"
		"  passwordvault <-set|-get|-list|-del> [-entry <entry> [-pass <password>]] [-field <field> [-value <value>]] \n"
		"             <vault-file> [-vaultpass <vault-password>]\n\n"
        "DESCRIPTION\n"
        "  PasswordVault demonstrates the usage of PasswordVault from SecureBlackbox.\n"
        "  Used to create vaults with user information.\n\n"
        "  The options are as follows:\n\n"
        "  -set          Whether to add new entry or add/modify field value.\n\n"
        "  -get          Whether to get field value.\n\n"
        "  -list         Whether to get list of entries or fields.\n\n"
        "  -del          Whether to remove entry or field value.\n\n"
        "  -entry        The entry name.\n\n"
        "  -pass         The password for the entry.\n\n"
        "  -field        The field name.\n\n"
        "  -value        The new value of field.\n\n"
		"  vault-file    The vault file (Required).\n\n"
        "  -vaultpass    The password for the vault.\n\n"
        "EXAMPLES\n"
        "	passwordvault -list myvault.bin\n\n"
        "	passwordvault -get -entry myftpserver -pass mypassword -field password myvault.bin\n\n"
        "	passwordvault -set -entry myftpserver -field username -value newusername myvault.bin\n\n"
        "	passwordvault -set -entry newentry myvault.bin -vaultpass mypassword\n\n"
        "	passwordvault -del -entry myftpserver -field username myvault.bin\n\n"
    );

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char** argv) {
    PasswordVault vault;
    char* vaultfile = "";
    char* vaultpass = "";
    char* entry = "";
    char* entrypass = "";
    char* field = "";
    char* value = "";


    boolean list = false;
    boolean get = false;
    boolean set = false;
    boolean del = false;

    // Validate input
    if (argc < 2) {
        displayHelp("");
        goto done;
    }

    int i = 1;
    while (i < argc) {
        if (argv[i][0] == '-') {
            if (!strcmp(argv[i], "-list")) list = true;
            else if (!strcmp(argv[i], "-get")) get = true;
            else if (!strcmp(argv[i], "-set")) set = true;
            else if (!strcmp(argv[i], "-del")) del = true;
            else if (!strcmp(argv[i], "-vaultpass")) {
                vaultpass = argv[i + 1];
                i++;
            } else if (!strcmp(argv[i], "-entry")) {
                entry = argv[i + 1];
                i++;
            } else if (!strcmp(argv[i], "-pass")) {
                entrypass = argv[i + 1];
                i++;
            } else if (!strcmp(argv[i], "-field")) {
                field = argv[i + 1];
                i++;
            } else if (!strcmp(argv[i], "-value")) {
                value = argv[i + 1];
                i++;
            }
        } else {
            vaultfile = argv[i];
        }

        i++;
    }

    if (!(list || get || set || del)) {
        displayHelp("-list or -get or -set or -del is required.");
        goto done;
    }

    if ((list && (get || set || del)) || (get && (list || set || del)) || (set && (get || list || del)) || (del && (get ||
        set || list))) {
        displayHelp("Use only one -list or -get or -set or -del parameter.");
        goto done;
    }

    if (!strcmp(vaultfile, "")) {
        displayHelp("vault-file is required.");
        goto done;
    }

    vault.SetPassword(vaultpass);

    boolean openVault = false;
    const boolean saveVault = set || del;
    if (set) {
        FILE* fp = fopen(vaultfile, "rb");
        if (fp) {
            openVault = true;
            fclose(fp);
        } else {
            openVault = false;
        }
    } else {
        openVault = true;
    }

    if (openVault && vault.OpenFile(vaultfile)) {
        goto err;
    }

    if (list) {
        if (!strcmp(entry, "")) {
            char* entries = vault.ListEntries();
            if (!entries) {
                goto err;
            }

            char* res = new char[1000];
            strcpy(res, "Entries: \n");
            strcat(res, entries);
			strcat(res, "\n\n");
            printf(res);
            delete[] res;
        } else {
            char* fields = vault.ListFields(entry, true);
            if (!fields) {
                goto err;
            }

            char* res = new char[1000];
            strcpy(res, "Fields in \"");
            strcat(res, entry);
            strcat(res, "\": \n");
            strcat(res, fields);
			strcat(res, "\n\n");
            printf(res);
            delete[] res;
        }
    } else if (get) {
        if (!strcmp(entry, "")) {
            displayHelp("-entry is required.");
            goto done;
        }

        if (!strcmp(field, "")) {
            displayHelp("-field is required.");
            goto done;
        }

        vault.SetEntryPassword(entrypass);

        value = vault.GetEntryValueStr(entry, field);
        if (!value) {
            goto err;
        }

        char* res = new char[1000];
        strcpy(res, "Value: ");
        strcat(res, value);
		strcat(res, "\n\n");
        printf(res);
        delete[] res;
    } else if (set) {
        if (!strcmp(entry, "")) {
            displayHelp("-entry is required.");
            goto done;
        }

        if (!strcmp(field, "")) {
            if (vault.AddEntry(entry)) {
                goto err;
            }
			vault.ChangeEntryPassword(entry, entrypass);

            char* res = new char[1000];
            strcpy(res, "Entry \"");
            strcat(res, entry);
            strcat(res, "\" successfully added.\n\n");
            printf(res);
            delete[] res;
        } else {
			// try add entry if not exist
			if (vault.AddEntry(entry) == 0) {
				vault.ChangeEntryPassword(entry, entrypass);
			}			

            vault.SetEntryPassword(entrypass);

            if (vault.SetEntryValueStr(entry, field, value, strcmp(entrypass, ""))) {
                goto err;
            }

            char* res = new char[1000];
            strcpy(res, "Field \"");
            strcat(res, field);
            strcat(res, "\" successfully added/modify.\n\n");
            printf(res);
            delete[] res;
        }
    } else {
        // del
        if (!strcmp(entry, "")) {
            displayHelp("-entry is required.");
            goto done;
        }

        if (!strcmp(field, "")) {
            if (vault.RemoveEntry(entry)) {
                goto err;
            }

            char* res = new char[1000];
            strcpy(res, "Entry \"");
            strcat(res, entry);
            strcat(res, "\" successfully removed.\n\n");
            printf(res);
            delete[] res;
        } else {
            if (vault.RemoveField(entry, field)) {
                goto err;
            }

            char* res = new char[1000];
            strcpy(res, "Field \"");
            strcat(res, field);
            strcat(res, "\" successfully removed.\n\n");
            printf(res);
            delete[] res;
        }
    }

    if (saveVault && vault.SaveFile(vaultfile)) {
        goto err;
    }

err:
    if (vault.GetLastErrorCode()) {
        printf("Error: [%i] %s\n", vault.GetLastErrorCode(), vault.GetLastError());
    }
done:
	printf("Press Enter to exit the demo.\n");
    getchar();
}


