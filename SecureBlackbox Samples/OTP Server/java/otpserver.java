/*
 * SecureBlackbox 2024 Java Edition - Sample Project
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

import java.io.*;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import java.nio.charset.StandardCharsets;
import secureblackbox.*;

public class otpserver extends ConsoleDemo {
	private static String optval(String[] args, String option) {
		for (int x = 0; x < args.length - 1; x++) {
			if (args[x].equalsIgnoreCase(option)) {
				return args[x + 1];
			}
		}
		return "";
	}

	private static boolean optext(String[] args, String option) {
		for (int x = 0; x < args.length; x++) {
			if (args[x].equalsIgnoreCase(option)) {
				return true;
			}
		}
		return false;
	}

	private static void displayHelp(String errMes) {
		System.out.println(
				"NAME\n" +
				"  otpserver -- SecureBlackbox OTPServer Demo Application\n\n" +
				"SYNOPSIS\n" +
				"  otpserver <-alg algorithm> [-secret key_secret] [-pass password] [-interval interval] [-delta delta]\n\n" +
				"DESCRIPTION\n" +
				"  This sample acts as a basic One-Time-Password protocol server. \n" +
				"  The options are as follows:\n\n" +
				"  -alg       The otp algorithm (Required). Valid values:\n\n" +
				"                0 - HOTP\n" +
				"                1 - TOTP\n\n" +
				"  -secret     The key secret (Required).\n\n" +
				"  -pass       The verifiable password (Required).\n\n" +
				"  -interval   The interval value.\n\n" +
				"  -delta      The delta.\n\n" +
				"EXAMPLES\n" +
				"  otpserver -alg 0 -secret testsecret -pass 0334587632\n\n" +
				"  otpserver -alg 1 -secret testsecret -pass 0334587632 -interval 56 -delta 4\n"
		);

		if (errMes.length() > 0) {
			System.out.println("Error: " + errMes);
			System.out.println();
		}

		confirmExit();
	}

	private static void confirmExit() {
		System.out.println("Press Enter to exit the demo.");
		input();
	}

	public static void main(String[] args) {
		if (args.length == 0) {
			displayHelp("");
			return;
		}

		OTPServer server = new OTPServer();
		int algorithm;
		int interval = 0;
		String secret;
		String pass;

		try {
			if (optext(args, "-alg")) {
				algorithm = Integer.parseInt(optval(args, "-alg"));
			} else {
				displayHelp("-alg is required.");
				return;
			}

			if (optext(args, "-secret")) {
				secret = optval(args, "-secret");
			} else {
				displayHelp("-secret is required.");
				return;
			}

			if (optext(args, "-pass")) {
				pass = optval(args, "-pass");
			} else {
				displayHelp("-pass is required.");
				return;
			}

			if (optext(args, "-interval")) {
				interval = Integer.parseInt(optval(args, "-interval"));
			}

			if (optext(args, "-delta")) {
				server.setDelta(Integer.parseInt(optval(args, "-delta")));
			}

			boolean res;
			switch (algorithm) {
				case 0:
					res = server.isHOTPPasswordValid(secret.getBytes(StandardCharsets.UTF_8), pass.length(), interval, pass);
					break;
				case 1:
					res = server.isTOTPPasswordValid(secret.getBytes(StandardCharsets.UTF_8), pass.length(), interval, "SHA256", pass);
					break;
				default:
					displayHelp("Invalid alg value.");
					return;
			}
			if (res)
				System.out.println("Password is valid");
			else
				System.out.println("Password is not valid");
			System.out.println();

			confirmExit();
		} catch (Exception ex) {
			ConsoleDemo.displayError(ex);
		}
	}
}

class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }
  static String prompt(String label, String punctuation, String defaultVal) {
      System.out.print(label + " [" + defaultVal + "] " + punctuation + " ");
      String response = input();
      if (response.equals(""))
        return defaultVal;
      else
        return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof SecureBlackboxException) {
      System.out.print(" (" + ((SecureBlackboxException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }
}




class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }
  static String prompt(String label, String punctuation, String defaultVal) {
      System.out.print(label + " [" + defaultVal + "]" + punctuation + " ");
      String response = input();
      if (response.equals(""))
        return defaultVal;
      else
        return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof SecureBlackboxException) {
      System.out.print(" (" + ((SecureBlackboxException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }

  /**
   * Takes a list of switch arguments or name-value arguments and turns it into a map.
   */
  static java.util.Map<String, String> parseArgs(String[] args) {
    java.util.Map<String, String> map = new java.util.HashMap<String, String>();
    
    for (int i = 0; i < args.length; i++) {
      // Add a key to the map for each argument.
      if (args[i].startsWith("-")) {
        // If the next argument does NOT start with a "-" then it is a value.
        if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
          // Save the value and skip the next entry in the list of arguments.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), args[i + 1]);
          i++;
        } else {
          // If the next argument starts with a "-", then we assume the current one is a switch.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), "");
        }
      } else {
        // If the argument does not start with a "-", store the argument based on the index.
        map.put(Integer.toString(i), args[i].toLowerCase());
      }
    }
    return map;
  }
}



