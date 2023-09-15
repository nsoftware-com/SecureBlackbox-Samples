import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;
import javax.swing.border.EmptyBorder;


public class adduserdialog extends JDialog {
	
	private static final long serialVersionUID = 1L;
	
	public JTextField edUserName;
	public JPasswordField tbPass;
	public JTextField edPublicKeyFile;
	public Boolean IsOk = false;

	String getOpenFileName(){
		JFileChooser fc = new JFileChooser();
		int returnVal = fc.showOpenDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION)
			return fc.getSelectedFile().getPath();

		return "";
	}

	public adduserdialog(){
		setTitle("Add user");
		setBounds(100, 100, 420, 210);
		getContentPane().setLayout(new BorderLayout());
		JPanel contentPanel = new JPanel();
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);		
		
		
		JLabel lbl = new JLabel();
		lbl.setText("Name");
		lbl.setBounds(10, 23, 67, 14);
		contentPanel.add(lbl);
		
		edUserName = new JTextField();
		edUserName.setBounds(90, 23, 310, 20);
		contentPanel.add(edUserName);		
		
		lbl = new JLabel();
		lbl.setText("Password");
		lbl.setBounds(10, 46, 67, 14);
		contentPanel.add(lbl);
		
		tbPass = new JPasswordField();
		tbPass.setBounds(90, 46, 310, 20);
		contentPanel.add(tbPass);

		JLabel lblPublicKeyFile = new JLabel("Public key file KEY authentication type");
		lblPublicKeyFile.setBounds(10, 80, 284, 14);
		contentPanel.add(lblPublicKeyFile);

		edPublicKeyFile = new JTextField();
		edPublicKeyFile.setBounds(10, 100, 300, 20);
		contentPanel.add(edPublicKeyFile);
		edPublicKeyFile.setColumns(10);

		JButton btnlPublicKeyFile = new JButton("Browse");
		btnlPublicKeyFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				edPublicKeyFile.setText(getOpenFileName());
			}
		});
		btnlPublicKeyFile.setBounds(320, 97, 80, 25);
		contentPanel.add(btnlPublicKeyFile);

		JPanel buttonPane = new JPanel();
		buttonPane.setLayout(new FlowLayout(FlowLayout.RIGHT));
		getContentPane().add(buttonPane, BorderLayout.SOUTH);
		{
			JButton okButton = new JButton("OK");
			okButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					IsOk = true;
					setVisible(false);
				}
			});
			okButton.setActionCommand("OK");
			buttonPane.add(okButton);
			getRootPane().setDefaultButton(okButton);
		}
		{
			JButton cancelButton = new JButton("Cancel");
			cancelButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					IsOk = false;
					setVisible(false);
				}
			});
			cancelButton.setActionCommand("Cancel");
			buttonPane.add(cancelButton);
		}
	}

}
