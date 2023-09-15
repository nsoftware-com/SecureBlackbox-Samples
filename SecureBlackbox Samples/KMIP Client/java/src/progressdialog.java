import java.awt.BorderLayout;
import java.awt.FlowLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

public class progressdialog extends JDialog {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private final JPanel contentPanel = new JPanel();
	private JLabel lSrc;
	private JLabel lblDestinationFile;
	private JLabel lDst;
	private JLabel lblProcessed;
	private JLabel lProcessed;
	private JProgressBar progressBar;

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
		setTitle("Download");
		setBounds(100, 100, 450, 215);
		getContentPane().setLayout(new BorderLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		contentPanel.setLayout(null);
		{
			JPanel panel = new JPanel();
			panel.setBorder(new TitledBorder(null, "Progress", TitledBorder.LEADING, TitledBorder.TOP, null, null));
			panel.setBounds(10, 11, 422, 133);
			contentPanel.add(panel);
			panel.setLayout(null);
			{
				JLabel lblSourceFile = new JLabel("Source file:");
				lblSourceFile.setBounds(10, 28, 81, 14);
				panel.add(lblSourceFile);
			}
			{
				lSrc = new JLabel(" ");
				lSrc.setBounds(101, 28, 311, 14);
				panel.add(lSrc);
			}
			{
				lblDestinationFile = new JLabel("Destination file: ");
				lblDestinationFile.setBounds(10, 53, 97, 14);
				panel.add(lblDestinationFile);
			}
			{
				lDst = new JLabel(" ");
				lDst.setBounds(99, 53, 313, 14);
				panel.add(lDst);
			}
			
			progressBar = new JProgressBar();
			progressBar.setBounds(10, 78, 402, 16);
			panel.add(progressBar);
			
			lblProcessed = new JLabel("Processed:");
			lblProcessed.setBounds(10, 105, 81, 14);
			panel.add(lblProcessed);
			
			lProcessed = new JLabel(" ");
			lProcessed.setBounds(101, 105, 311, 14);
			panel.add(lProcessed);
		}
		{
			JPanel buttonPane = new JPanel();
			buttonPane.setLayout(new FlowLayout(FlowLayout.RIGHT));
			getContentPane().add(buttonPane, BorderLayout.SOUTH);
			{
				JButton cancelButton = new JButton("Cancel");
				cancelButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						setVisible(false);
					}
				});
				cancelButton.setActionCommand("Cancel");
				buttonPane.add(cancelButton);
			}
		}
	}

	public void setCaptionText(String cap) {
		setTitle(cap);
		
	}

	public void setSourceFilename(String path) {
		lSrc.setText(path);
	}

	public void setDestFilename(String path) {
		lDst.setText(path);
		
	}

	public void setProgressText(String s) {
		lProcessed.setText(s);
		
	}

	public void setProgressValue(int i) {
		progressBar.setValue(i);
	}
}
