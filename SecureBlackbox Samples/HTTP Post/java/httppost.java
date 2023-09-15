/*
 * SecureBlackbox 2022 Java Edition - Sample Project
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
import java.awt.*;
import javax.swing.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.TooManyListenersException;

import secureblackbox.*;

public class httppost extends JDialog {

	private static final long serialVersionUID = 1L;
	private JTextField edURL;
	private JTextField edPath;
	private JCheckBox cbPostAsForm;
	private Httpclient HTTPClient;
	private JTextArea mmLog;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					httppost dialog = new httppost();
					dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
					dialog.setVisible(true);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	/**
	 * Create the dialog.
	 */
	public httppost() {
		setTitle("HTTP Post");
		setBounds(100, 100, 460, 350);
		getContentPane().setLayout(null);

		JLabel lblCaption = new JLabel("This sample illustrates POST request capabilities of HTTPClient control.");
		lblCaption.setBounds(10, 5, 450, 14);
		lblCaption.setForeground(new Color(49, 106, 197));
		getContentPane().add(lblCaption);

		JLabel lblHost = new JLabel("URL");
		lblHost.setBounds(10, 28, 61, 14);

		edURL = new JTextField();
		edURL.setText("http://localhost/upload/simple_upload.php");
		edURL.setBounds(45, 25, 310, 20);
		getContentPane().add(edURL);
		edURL.setColumns(10);
		getContentPane().add(lblHost);

		JLabel lblPath = new JLabel("Path");
		lblPath.setBounds(10, 58, 61, 14);
		getContentPane().add(lblPath);

		edPath = new JTextField();
		edPath.setBounds(45, 55, 310, 20);
		getContentPane().add(edPath);
		edPath.setColumns(10);

		JButton btnBrowse = new JButton("Browse");
		btnBrowse.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edPath.setText(getSaveFileName());
			}
		});
		btnBrowse.setBounds(360, 53, 80, 25);
		getContentPane().add(btnBrowse);

		cbPostAsForm = new JCheckBox("Post as form");
		cbPostAsForm.setBounds(250, 93, 100, 20);
		getContentPane().add(cbPostAsForm);

		JButton btnRetrieve = new JButton("Post");
		btnRetrieve.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				postClick();
			}
		});
		btnRetrieve.setBounds(360, 90, 80, 25);
		getContentPane().add(btnRetrieve);

		JScrollPane scrollPane = new JScrollPane();
		scrollPane.setBounds(10, 120, 430, 185);
		getContentPane().add(scrollPane);

		mmLog = new JTextArea();
		scrollPane.setViewportView(mmLog);

		init();
	}

	private void init() {
		HTTPClient = new Httpclient();

		initEvents();
	}

	private void initEvents() {
		try {
			HTTPClient.addHttpclientEventListener(new HttpclientEventListener() {
				
				@Override
				public void supercoreIntercept(HttpclientSupercoreInterceptEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void redirection(HttpclientRedirectionEvent e) {
					mmLog.append("\nRedirected to " + e.newURL + "\n");
					e.newURL = e.newURL; // can be changed here
					e.allowRedirection = true;
				}
				
				@Override
				public void progress(HttpclientProgressEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void headersReceived(HttpclientHeadersReceivedEvent e) {
					mmLog.append("\nReceived headers: \n");
					StringNameValuePair element;
					for (int i = 0; i < HTTPClient.getResponseHeaders().size(); i++)
					{
						element = HTTPClient.getResponseHeaders().item(i);
						mmLog.append(element.getName() + ": " + element.getValue() + "\n");
					}
				}
				
				@Override
				public void headersPrepared(HttpclientHeadersPreparedEvent e) {
					mmLog.append("\nSending headers: \n");
					StringNameValuePair element;
					for (int i = 0; i < HTTPClient.getRequestHeaders().size(); i++)
					{
						element = HTTPClient.getRequestHeaders().item(i);
						mmLog.append(element.getName() + ": " + element.getValue() + "\n");
					}
				}
				
				@Override
				public void externalSign(HttpclientExternalSignEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void error(HttpclientErrorEvent e) {
					mmLog.append(e.errorCode + ": " + e.description + "\n");
				}

				@Override
				public void notification(HttpclientNotificationEvent e) {
					// TODO Auto-generated method stub
				}

				@Override
				public void dynamicDataNeeded(HttpclientDynamicDataNeededEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void documentEnd(HttpclientDocumentEndEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void documentBegin(HttpclientDocumentBeginEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void cookie(HttpclientCookieEvent e) {
					// TODO Auto-generated method stub
					
				}
				
				@Override
				public void certificateValidate(HttpclientCertificateValidateEvent e) {
					e.accept = true;
				}
			});
		}
		catch (TooManyListenersException ex)
		{

		}
	}

	private void showErrorMessage(String msg) {
		JOptionPane.showMessageDialog(null, msg, getTitle(), JOptionPane.ERROR_MESSAGE);
	}

	String getSaveFileName(){
		JFileChooser fc = new JFileChooser();
		int returnVal = fc.showSaveDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION)
			return fc.getSelectedFile().getPath();

		return "";
	}

	protected void postClick() {
		if (edPath.getText().length() > 0 && edURL.getText().length() > 0)
		{
			try
			{
				if (cbPostAsForm.isSelected())
					HTTPClient.postWebForm(edURL.getText(), "upload=Upload", "userfile", edPath.getText(), "");
				else
					HTTPClient.postFile(edURL.getText(), edPath.getText());
			}
			catch (Exception E)
			{
				showErrorMessage("Exception happened during HTTP post: " + E.getMessage());
			}
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

  static String prompt(String label, String punctuation, String defaultVal)
  {
	System.out.print(label + " [" + defaultVal + "] " + punctuation + " ");
	String response = input();
	if(response.equals(""))
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



