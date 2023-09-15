import java.awt.BorderLayout;
import java.awt.FlowLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.JPasswordField;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.io.*;
import secureblackbox.*;

public class certificatedialog extends JDialog {
	private static final long serialVersionUID = 7409564391351472542L;
	private final JPanel contentPanel = new JPanel();
	private JTextField textField;
	private JPasswordField passwordField;
	Certificatemanager certmanager;
	
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			certificatedialog dialog = new certificatedialog();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public certificatedialog() {
		certmanager = new Certificatemanager();
		
		setTitle("Private key needed");
		setBounds(100, 100, 450, 139);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);
		
		JLabel lblNewLabel = new JLabel("Certificate file");
		lblNewLabel.setBounds(10, 11, 86, 14);
		contentPanel.add(lblNewLabel);
		
		textField = new JTextField();
		textField.setBounds(116, 8, 215, 20);
		contentPanel.add(textField);
		textField.setColumns(10);
		
		JButton btnChoose = new JButton("Choose...");
		btnChoose.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				textField.setText(getFileName());
			}
		});
		btnChoose.setBounds(341, 7, 91, 23);
		contentPanel.add(btnChoose);
		
		JLabel lblCertificatePassword = new JLabel("Certificate password");
		lblCertificatePassword.setBounds(10, 36, 122, 14);
		contentPanel.add(lblCertificatePassword);
		
		passwordField = new JPasswordField();
		passwordField.setBounds(142, 33, 188, 20);
		contentPanel.add(passwordField);
		{
			JPanel buttonPane = new JPanel();
			buttonPane.setLayout(new FlowLayout(FlowLayout.RIGHT));
			getContentPane().add(buttonPane, BorderLayout.SOUTH);
			{
				JButton okButton = new JButton("Load");
				okButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						loadCertificate();
					}
				});
				okButton.setActionCommand("OK");
				buttonPane.add(okButton);
				getRootPane().setDefaultButton(okButton);
			}
			{
				JButton cancelButton = new JButton("Cancel");
				cancelButton.setActionCommand("Cancel");
				buttonPane.add(cancelButton);
			}
		}
	}

	private void loadCertificate() {
        try
        {
			certmanager.importFromFile(textField.getText(), new String(passwordField.getPassword()));

			if (certmanager.getCertificate().getPrivateKeyExists())
            {
                dispose();
            }
            else
            	showMessage("Cannot load certificate", "There is no private key for this certificate.");
        }
        catch (Exception ex)
        {
        	showMessage("Error", ex.getMessage());
        }
	}
	
	static void showMessage(String caption, String msg) {
		JOptionPane.showMessageDialog(null, msg, caption, JOptionPane.INFORMATION_MESSAGE);
	}	

	public Certificate getCertificate() {
		return certmanager.getCertificate();
	}
	
	String getFileName(){
		JFileChooser fc = new JFileChooser();

	    int returnVal = fc.showOpenDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION)
	        return fc.getSelectedFile().getPath();

	    return "";
	} 
}
