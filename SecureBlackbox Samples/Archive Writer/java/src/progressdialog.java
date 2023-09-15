import java.awt.BorderLayout;
import java.awt.FlowLayout;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.Date;

public class progressdialog extends JDialog {
	private static final long serialVersionUID = 422956728888165847L;
	private final JPanel contentPanel = new JPanel();
	private JTable table;
	JButton okButton;
	JButton cancelButton;
	boolean cancelOperation;
	progresstablemodel model;
	JLabel lblCurrentFile;
	JProgressBar progressCur;
	JProgressBar progressTotal;
	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			progressdialog dialog = new progressdialog();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public progressdialog() {
		setTitle("Operation progress");
		setBounds(100, 100, 450, 300);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);
		
		JLabel lblCurrent = new JLabel("Current");
		lblCurrent.setBounds(10, 11, 46, 14);
		contentPanel.add(lblCurrent);
		
		progressCur = new JProgressBar();
		progressCur.setMaximum(100000);
		progressCur.setBounds(66, 11, 366, 16);
		contentPanel.add(progressCur);
		
		JLabel lblTotal = new JLabel("Total");
		lblTotal.setBounds(10, 36, 46, 14);
		contentPanel.add(lblTotal);
		
		progressTotal = new JProgressBar();
		progressTotal.setMaximum(100000);
		progressTotal.setBounds(66, 36, 366, 16);
		contentPanel.add(progressTotal);
		
		JScrollPane scrollPane = new JScrollPane();
		scrollPane.setBounds(10, 77, 422, 152);
		contentPanel.add(scrollPane);
		
		table = new JTable();
		model = new progresstablemodel();
		table.setModel(model);
		
		scrollPane.setViewportView(table);
		
		lblCurrentFile = new JLabel(" ");
		lblCurrentFile.setBounds(10, 61, 422, 14);
		contentPanel.add(lblCurrentFile);
		{
			JPanel buttonPane = new JPanel();
			buttonPane.setLayout(new FlowLayout(FlowLayout.RIGHT));
			getContentPane().add(buttonPane, BorderLayout.SOUTH);
			{
				okButton = new JButton("OK");
				okButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent arg0) {
						setVisible(false);
					}
				});
				okButton.setActionCommand("OK");
				buttonPane.add(okButton);
				getRootPane().setDefaultButton(okButton);
			}
			{
				cancelButton = new JButton("Cancel");
				cancelButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						cancelOperation = true;
						setVisible(false);
					}
				});
				cancelButton.setActionCommand("Cancel");
				buttonPane.add(cancelButton);
			}
		}
	}

	public void setCaptionText(String cap) {
		this.setTitle(cap);
	}

	public void setButtonOKEnabled(boolean b) {
		okButton.setEnabled(b);		
	}

	public void setButtonCancelEnabled(boolean b) {
		cancelButton.setEnabled(b);
	}

	public void setCancelOperation(boolean b) {
		cancelOperation = b;
	}
	
	public boolean getCancelOperation(){
		return cancelOperation;
	}

	public void clearLog() {
		model.clear();
	}

	public void addToLog(String msg) {
		model.addRow(new Date(), msg);
	}

	public void setCurrentFileName(String path) {
		lblCurrentFile.setText(path);
	}

	public void setProgressBarCurrentFile(int i) {
		progressCur.setValue(i);
	}

	public void setProgressBarTotal(int i) {
		progressTotal.setValue(i);
	}
}
