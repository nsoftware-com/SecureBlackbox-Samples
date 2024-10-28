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
        "  usermanager -- SecureBlackbox UserManager Demo Application\n\n"
        "SYNOPSIS\n"
		"  usermanager <-set|-check|-list|-del> <users-file> <-filepass file-password> [-user user_name] [-pass password]\n\n"
        "DESCRIPTION\n"
        "  UserManager demonstrates the usage of UserManager from SecureBlackbox.\n"
        "  Used to create file with user information.\n\n"
        "  The options are as follows:\n\n"
        "  -set          Whether to add/modify user.\n\n"
        "  -check        Whether to verify user password.\n\n"
        "  -list         Whether to get list of users.\n\n"
        "  -del          Whether to remove user.\n\n"
		"  users-file    The users file (Required).\n\n"
        "  -filepass     The password for the file (Required).\n\n"
        "  -user         The user name\n\n"
        "  -pass         The user's password.\n\n"
        "EXAMPLES\n"
		"	usermanager -list myusers.bin -filepass password\n\n"
		"	usermanager -check myusers.bin -filepass password -user myuser -pass mypassword \n\n"
		"	usermanager -set myusers.bin -filepass password -user myuser -pass newpass \n\n"
		"	usermanager -set myusers.bin -filepass password -user myuser \n\n"
		"	usermanager -del myusers.bin -filepass password -user myuser \n\n"
    );

	if (strlen(errMes) > 0)
	{
		printf("Error: %s \n\n", errMes);
	}
}

int main(int argc, char** argv) {
    UserManager um;
    char* filename = "";
    char* filepass = "";
    char* user = "";
    char* pass = "";

    boolean list = false;
    boolean check = false;
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
            else if (!strcmp(argv[i], "-check")) check = true;
            else if (!strcmp(argv[i], "-set")) set = true;
            else if (!strcmp(argv[i], "-del")) del = true;
            else if (!strcmp(argv[i], "-filepass")) {
				filepass = argv[i + 1];
                i++;
            } else if (!strcmp(argv[i], "-user")) {
				user = argv[i + 1];
                i++;
			}
			else if (!strcmp(argv[i], "-pass")) {
				pass = argv[i + 1];
				i++;
			}
        } else {
			filename = argv[i];
        }

        i++;
    }

    if (!(list || check || set || del)) {
        displayHelp("-list or -check or -set or -del is required.");
        goto done;
    }

    if ((list && (check || set || del)) || (check && (list || set || del)) || (set && (check || list || del)) || 
		(del && (check || set || list))) {
        displayHelp("Use only one -list or -check or -set or -del parameter.");
        goto done;
    }

    if (!strcmp(filename, "")) {
        displayHelp("users-file is required.");
        goto done;
    }

	if (!strcmp(filepass, "")) {
		displayHelp("filepass is required.");
		goto done;
	}

    boolean openFile = false;
    const boolean saveFile = set || del;
    if (set) {
        FILE* fp = fopen(filename, "rb");
        if (fp) {
			openFile = true;
            fclose(fp);
        } else {
			openFile = false;
        }
    } else {
		openFile = true;
    }

    if (openFile && um.ImportFromFile(filename, filepass, true)) {
        goto err;
    }

    if (list) {
		printf("%d user(s) have been found\n\n", um.GetUserCount());

		for (int x = 0; x < um.GetUserCount(); x++) {
			printf("- User %d: %s (%s)\n\n", (x + 1), um.GetUserUsername(x), (strlen(um.GetUserPassword(x)) > 0) ? "with password" : "no password");			
		}
    } else if (check) {
        if (!strcmp(user, "")) {
            displayHelp("-user is required.");
            goto done;
        }

        if (!strcmp(pass, "")) {
            displayHelp("-pass is required.");
            goto done;
        }

		if (um.VerifyUser(user, pass))
		{
			printf("Password is correct\n\n");
		}
		else
		{
			printf("Password is incorrect\n\n");
		}
    } else if (set) {
        if (!strcmp(user, "")) {
            displayHelp("-user is required.");
            goto done;
        }

		int Idx = -1;
		for (int x = 0; x < um.GetUserCount(); x++) {
			if (strcmp(user, um.GetUserUsername(x)) == 0) {
				Idx = x;
				break;
			}
		}

		if (Idx == -1)
		{
			Idx = um.AddUser(user);
			printf("New user added\n\n");
		}
		else
		{
			printf("User has been changed\n\n");
		}

		um.SetUserPassword(Idx, pass);
	}
	else {
		// del
		if (!strcmp(user, "")) {
			displayHelp("-user is required.");
			goto done;
		}

		int userNum = -1;
		for (int x = 0; x < um.GetUserCount(); x++) {
			if (strcmp(user, um.GetUserUsername(x)) == 0) {
				userNum = x;
				break;
			}
		}

		if (userNum >= 0)
		{
			for (int x = userNum; x < um.GetUserCount() - 1; x++) {
				um.SetUserHandle(x, um.GetUserHandle(x + 1));
			}
			um.SetUserCount(um.GetUserCount() - 1);
			printf("User has been deleted\n\n");
		}
		else
		{
			printf("User not found\n\n");
		}
	}

    if (saveFile && um.ExportToFile(filename, filepass)) {
        goto err;
    }

err:
    if (um.GetLastErrorCode()) {
        printf("Error: [%i] %s\n\n", um.GetLastErrorCode(), um.GetLastError());
    }
done:
    printf("Press Enter to exit the demo.\n");
    getchar();
}


