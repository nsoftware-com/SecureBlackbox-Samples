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
		setBounds(100, 100, 420, 160);
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
