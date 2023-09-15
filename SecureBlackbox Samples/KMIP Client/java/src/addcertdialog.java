import java.awt.BorderLayout;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

public class addcertdialog extends JDialog {

    private static final long serialVersionUID = 1L;
    private final JPanel contentPanel = new JPanel();
    private JTextField tbCertFile;
    private JTextField tbPassword;
    private JTextField tbId;

    protected boolean isOk;

    /**
     * Launch the application.
     */
    public static void main(String[] args) {
        try {
            conndialog dialog = new conndialog();
            dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
            dialog.setVisible(true);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Create the dialog.
     */
    public addcertdialog() {
        setTitle("Adding certificate");
        setBounds(100, 100, 455, 205);
        getContentPane().setLayout(new BorderLayout());
        contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
        getContentPane().add(contentPanel, BorderLayout.CENTER);
        contentPanel.setLayout(null);

        JLabel lblCertFile = new JLabel("Certificate file");
        lblCertFile.setBounds(10, 28, 90, 14);
        contentPanel.add(lblCertFile);

        tbCertFile = new JTextField("");
        tbCertFile.setBounds(100, 25, 240, 20);
        contentPanel.add(tbCertFile);
        tbCertFile.setColumns(10);

        JButton btnKeyFile = new JButton("Browse");
        btnKeyFile.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                tbCertFile.setText(getOpenFileName());
            }
        });
        btnKeyFile.setBounds(350, 23, 80, 25);
        contentPanel.add(btnKeyFile);

        JLabel lblPassword = new JLabel("Password");
        lblPassword.setBounds(10, 58, 60, 14);
        contentPanel.add(lblPassword);

        tbPassword = new JTextField();
        tbPassword.setText("");
        tbPassword.setBounds(75, 55, 158, 20);
        contentPanel.add(tbPassword);

        JLabel lblId = new JLabel("Id");
        lblId.setBounds(10, 88, 20, 14);
        contentPanel.add(lblId);

        tbId = new JTextField();
        tbId.setText("");
        tbId.setBounds(30, 85, 158, 20);
        contentPanel.add(tbId);


        JButton btnOk = new JButton("Ok");
        btnOk.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                isOk = true;
                setVisible(false);
            }
        });
        btnOk.setBounds(260, 135, 80, 25);
        contentPanel.add(btnOk);

        JButton btnCancel = new JButton("Cancel");
        btnCancel.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                isOk = false;
                setVisible(false);
            }
        });
        btnCancel.setBounds(350, 135, 80, 25);
        contentPanel.add(btnCancel);
    }

    String getOpenFileName(){
        JFileChooser fc = new JFileChooser();
        int returnVal = fc.showOpenDialog(this);
        if (returnVal == JFileChooser.APPROVE_OPTION)
            return fc.getSelectedFile().getPath();

        return "";
    }

    public boolean isOK() {
        return isOk;
    }

    public String getCertFile() {
        return tbCertFile.getText();
    }

    public String getPassword() {
        return tbPassword.getText();
    }

    public String getId() {
        return tbId.getText();
    }
}
