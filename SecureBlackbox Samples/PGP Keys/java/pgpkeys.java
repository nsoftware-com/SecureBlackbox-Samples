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
import java.net.URL;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JMenuBar;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPasswordField;
import javax.swing.JSeparator;
import javax.swing.JToolBar;
import javax.swing.JButton;
import javax.swing.JTree;
import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.event.TreeSelectionEvent;

import secureblackbox.*;

public class pgpkeys extends JDialog {

	private static final long serialVersionUID = 1L;
	private String publicKeyringFile = "";
	private String secretKeyringFile = "";
	private Pgpkeyring keyring;
	private JLabel lStatus;
	private JTree tree;
	DefaultMutableTreeNode rootNode;

	private JPanel pKeyInfo;
	private JLabel lKeyAlgorithm;
	private JLabel lKeyID;
	private JLabel lKeyFP;
	private JLabel lTimestamp;
	private JLabel lTrust;
	private JLabel lExpires;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					pgpkeys dialog = new pgpkeys();
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
	public pgpkeys() {
		addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent e) {
				close();
			}
		});			
		
		setTitle("PGPKeys Demo Application");
		setResizable(false);
		setBounds(100, 100, 740, 455);
		getContentPane().setLayout(null);

		JLabel lblCaption = new JLabel("This sample is a simple OpenPGP keyring manager. ");
		lblCaption.setBounds(10, 5, 730, 14);
		lblCaption.setForeground(new Color(49, 106, 197));
		getContentPane().add(lblCaption);
		
		JToolBar toolBar = new JToolBar();
		toolBar.setFloatable(false);
		toolBar.setBounds(5, 25, 725, 23);
		getContentPane().add(toolBar);
		
		JButton tNew = new JButton(" ");
		tNew.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				NewKeyring();
			}
		});
		tNew.setSize(new Dimension(20, 20));
		tNew.setIgnoreRepaint(true);
		tNew.setRequestFocusEnabled(false);
		tNew.setFocusable(false);
		tNew.setIcon(getIcon("new"));
		tNew.setFocusPainted(false);
		toolBar.add(tNew);
		
		JButton tLoad = new JButton(" ");
		tLoad.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				LoadKeyring();
			}
		});
		tLoad.setSize(new Dimension(20, 20));
		tLoad.setIgnoreRepaint(true);
		tLoad.setRequestFocusEnabled(false);
		tLoad.setFocusable(false);
		tLoad.setIcon(getIcon("load"));
		tLoad.setFocusPainted(false);
		toolBar.add(tLoad);
		
		JButton tSave = new JButton(" ");
		tSave.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				SaveKeyring();
			}
		});
		tSave.setSize(new Dimension(20, 20));
		tSave.setIgnoreRepaint(true);
		tSave.setRequestFocusEnabled(false);
		tSave.setFocusable(false);
		tSave.setIcon(getIcon("save"));
		tSave.setFocusPainted(false);
		toolBar.add(tSave);
		
		JButton tGenerate = new JButton(" ");
		tGenerate.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				generateKey();
			}
		});
		tGenerate.setSize(new Dimension(20, 20));
		tGenerate.setIgnoreRepaint(true);
		tGenerate.setRequestFocusEnabled(false);
		tGenerate.setFocusable(false);
		tGenerate.setIcon(getIcon("gen"));
		tGenerate.setFocusPainted(false);
		toolBar.add(tGenerate);
		
		JButton tAdd = new JButton(" ");
		tAdd.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				AddKey();
			}
		});
		tAdd.setSize(new Dimension(20, 20));
		tAdd.setIgnoreRepaint(true);
		tAdd.setRequestFocusEnabled(false);
		tAdd.setFocusable(false);
		tAdd.setIcon(getIcon("add"));
		tAdd.setFocusPainted(false);
		toolBar.add(tAdd);
		
		JButton tRemove = new JButton(" ");
		tRemove.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				RemoveKey();
			}
		});
		tRemove.setSize(new Dimension(20, 20));
		tRemove.setIgnoreRepaint(true);
		tRemove.setRequestFocusEnabled(false);
		tRemove.setFocusable(false);
		tRemove.setIcon(getIcon("remove"));
		tRemove.setFocusPainted(false);
		toolBar.add(tRemove);
		
		JButton tExport = new JButton(" ");
		tExport.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				ExportKey();
			}
		});
		tExport.setSize(new Dimension(20, 20));
		tExport.setIgnoreRepaint(true);
		tExport.setRequestFocusEnabled(false);
		tExport.setFocusable(false);
		tExport.setIcon(getIcon("export"));
		tExport.setFocusPainted(false);
		toolBar.add(tExport);

		rootNode = new DefaultMutableTreeNode();
		tree = new JTree(rootNode);
		tree.addTreeSelectionListener(new TreeSelectionListener() {
			public void valueChanged(TreeSelectionEvent e) {
				treeSelectClick(e);
			}
		});
		tree.setRootVisible(false);
		tree.setShowsRootHandles(true); 		
		tree.setBorder(new TitledBorder(null, "", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		tree.setBounds(5, 50, 725, 260);
		getContentPane().add(tree);
		
		pKeyInfo = new JPanel();
		pKeyInfo.setBounds(5, 315, 725, 80);
		getContentPane().add(pKeyInfo);
		pKeyInfo.setLayout(null);
		
		lKeyAlgorithm = new JLabel("Algorithm:");
		lKeyAlgorithm.setBounds(36, 11, 320, 14);
		pKeyInfo.add(lKeyAlgorithm);
		
		lKeyID = new JLabel("Key ID:");
		lKeyID.setBounds(36, 36, 320, 14);
		pKeyInfo.add(lKeyID);
		
		lKeyFP = new JLabel("Key FP:");
		lKeyFP.setBounds(36, 61, 320, 14);
		pKeyInfo.add(lKeyFP);
		
		lTimestamp = new JLabel("Created:");
		lTimestamp.setBounds(415, 11, 300, 14);
		pKeyInfo.add(lTimestamp);
		
		lExpires = new JLabel("Expires:");
		lExpires.setBounds(415, 36, 320, 14);
		pKeyInfo.add(lExpires);
		
		lTrust = new JLabel("Trust:");
		lTrust.setBounds(415, 61, 320, 14);
		pKeyInfo.add(lTrust);

		lStatus = new JLabel("Ready");
		lStatus.setBounds(5, 405, 735, 20);
		getContentPane().add(lStatus);

		init();
	}
	
	protected void treeSelectClick(TreeSelectionEvent e) {
		DefaultMutableTreeNode node = getSelectedNode(e.getPaths());
		nodedata data = (nodedata)node.getUserObject();
		Object tag = data.getTag();
		
		DrawPublicKeyProps((PGPKey)tag);
	}

	@SuppressWarnings("deprecation")
	private void DrawPublicKeyProps(PGPKey key)
	{
		HideAllInfoPanels();
		lKeyAlgorithm.setText("Algorithm: " + key.getPublicKeyAlgorithm() + " (" + key.getBitsInKey() + " bits)");
		lKeyID.setText("KeyID: " + key.getKeyID());
		lKeyFP.setText("KeyFP: " + key.getKeyFP());
		lTimestamp.setText("Created: " + key.getTimestamp());
		if (key.getExpires() == 0) 
		{
			lExpires.setText("Expires: NEVER");
		} 
		else 
		{
			lExpires.setText("Expires: " + key.getExpires());
		}
		pKeyInfo.setVisible(true);
	}

	protected void close() {
		try
		{
			keyring.close();
		}
		catch (Exception e)
		{}
		dispose();
		System.exit(0);
	}

	private void init() {
		keyring = new Pgpkeyring();
		try
		{
			keyring.addPgpkeyringEventListener(new PgpkeyringEventListener() {
				@Override
				public void error(PgpkeyringErrorEvent e) {
					String S = e.errorCode + e.description;
				}

				@Override
				public void supercoreIntercept(PgpkeyringSupercoreInterceptEvent e) {

				}
			});

			keyring.createNew();
		}
		catch (Exception e)
		{}
		HideAllInfoPanels();
		Status("Application started");		
	}

	protected void redrawKeyring() {
		utils.RedrawKeyring(tree, keyring, rootNode);
	}

	protected void Status(String msg) {
		lStatus.setText(msg);
	}

	private Icon getIcon(String name) {
		URL resPath = getClass().getResource("/" + name + ".ico");
		if (resPath == null) return new ImageIcon();
		return new ImageIcon(resPath);
	}
	
	private void generateKey() {
		generatekeydialog gd = new generatekeydialog();
		gd.setModal(true);
		gd.setVisible(true);
		if (gd.isSuccess()) 
		{
			try
			{
				keyring.setPinnedKey(gd.getNewKey());
				keyring.addPinned();
				redrawKeyring();
			}
			catch (Exception ex)
			{
				showErrorMessage(ex.getMessage(), "Error on added key");
			}
		}
		Status("New key was added to keyring");				
	}
	
	private void LoadKeyring()
	{
		loadsavekeyringdialog dlg = new loadsavekeyringdialog();
		dlg.setOpenDialog(true);
		dlg.setTitle("Load keyring");
		dlg.setModal(true);
		dlg.setVisible(true);
		if (dlg.isOK()) 
		{
			try 
			{
				if (keyring.isOpened())
					keyring.close();

				publicKeyringFile = dlg.getPublicKeyringText();
				secretKeyringFile = dlg.getSecretKeyringText();
				keyring.load(publicKeyringFile, secretKeyringFile);
			} 
			catch(Exception ex) 
			{
				showErrorMessage(ex.getMessage(), "Keyring error");
				Status("Failed to load keyring");
				return;
			}
			HideAllInfoPanels();
			redrawKeyring();
			Status("Keyring loaded");
		}
	}

	private void HideAllInfoPanels() {
		pKeyInfo.setVisible(false);
		getContentPane().repaint();
	}

	private void showErrorMessage(String msg, String cap) {
		JOptionPane.showMessageDialog(null, msg, cap, JOptionPane.ERROR_MESSAGE);				
	}

	private void SaveKeyring()
	{
		loadsavekeyringdialog dlg = new loadsavekeyringdialog();
		dlg.setOpenDialog(false);
		dlg.setTitle("Save keyring");
		dlg.setPublicKeyringText(publicKeyringFile);
		dlg.setSecretKeyringText(secretKeyringFile);
		dlg.setModal(true);
		dlg.setVisible(true);
		if (dlg.isOK()) { 		
			try 
			{
				publicKeyringFile = dlg.getPublicKeyringText();
				secretKeyringFile = dlg.getSecretKeyringText();
				keyring.save(publicKeyringFile, secretKeyringFile);

				Status("Keyring saved");
			} 
			catch(Exception ex) 
			{
				showErrorMessage(ex.getMessage(), "Keyring error");
				Status("Failed to save keyring");
			}
		}
	}

	private void NewKeyring() 
	{
		if (showConfirmMessage("Are you sure you want to create a new keyring?\nAll unsaved information will be LOST!", "New keyring")) 
		{
			try
			{
				publicKeyringFile = "";
				secretKeyringFile = "";
				keyring.close();
				keyring.createNew();

				Status("New keyring created");
			}
			catch(Exception ex)
			{
				showErrorMessage(ex.getMessage(), "Keyring error");
				Status("Failed to new keyring created");
			}

			HideAllInfoPanels();
			redrawKeyring();
		}
	}

	private boolean showConfirmMessage(String msg, String cap) {
		return (JOptionPane.showConfirmDialog(this, msg, cap, JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION);
	}

	private void AddKey()
	{
		String fileName = getOpenFileName();
		if (fileName.length() > 0)
		{
			try 
			{
				keyring.addFromFile(fileName);
			} 
			catch(Exception ex) 
			{
				showErrorMessage(ex.getMessage(), "Unable to load key");
				Status("Failed to import key");
				return;
			}

			redrawKeyring();
		}
		Status("Key(s) successfully imported");
	}

	protected String getOpenFileName() {
		JFileChooser fc = new JFileChooser();
		
		FileNameExtensionFilter filter = new FileNameExtensionFilter("PGP files", "pgp");
	    fc.setFileFilter(filter);	
	    
	    int returnVal = fc.showOpenDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION)
	        return fc.getSelectedFile().getPath();

	    return "";
	}

	protected String getSaveFileName() {
		JFileChooser fc = new JFileChooser();
		
		FileNameExtensionFilter filter = new FileNameExtensionFilter("PGP files", "pgp");
	    fc.setFileFilter(filter);	
	    
	    int returnVal = fc.showSaveDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION)
	        return fc.getSelectedFile().getPath();

	    return "";
	}
	
	private void RemoveKey()
	{
	    TreePath[] paths = tree.getSelectionPaths();

	    if (paths.length > 0) {
			nodedata node = getFirstNode(paths);

			PGPKey key = (PGPKey) (node.getTag());
			if (showConfirmMessage("Are you sure you want to remove the key (" + utils.GetDefaultUserID(key) +
					")?", "Remove key")) {
				if (key.getIsSecret())
				{
					if (showConfirmMessage("The key you want to remove is SECRET! Are you still sure?", "Remove key")) {
						return;
					}
				}
				try
				{
					keyring.removeByID(key.getKeyID());
					redrawKeyring();
					Status("Key was successfully removed");
				}
				catch(Exception ex)
				{
					showErrorMessage(ex.getMessage(), "Unable to remove key");
					Status("Failed to remove key");
					return;
				}
			}

		}
	}

	nodedata getFirstNode(TreePath[] paths){
		return (nodedata)((DefaultMutableTreeNode)paths[0].getLastPathComponent()).getUserObject();
	}

	private DefaultMutableTreeNode getSelectedNode(TreePath[] t) {
		return (DefaultMutableTreeNode)t[0].getLastPathComponent();
	}
	
	private void ExportKey()
	{
	    TreePath[] paths = tree.getSelectionPaths();
	    if (paths.length > 0)
	    {
			nodedata node = getFirstNode(paths);

			PGPKey key = (PGPKey) node.getTag();
			String fileName = getSaveFileName();
			if (fileName.length() > 0)
			{
				Pgpkeymanager keymanager = new Pgpkeymanager();
				try
				{
					keymanager.setKey(key);
					keymanager.exportToFile(fileName);
					Status("Key saved");
				}
				catch(Exception ex)
				{
					showErrorMessage(ex.getMessage(), "Unable to export key");
					Status("Failed to export key");
					return;
				}
			}

		}
	}

 	private String requestPassword(String msg) {
		JPasswordField jpf = new JPasswordField();
		int result = JOptionPane.showConfirmDialog(null, jpf, msg, JOptionPane.OK_CANCEL_OPTION);
		if (result == 0)
			return new String(jpf.getPassword());
		return "";
	}

	private String requestPassphrase(PGPKey key)
	{
		String msg = utils.GetDefaultUserID(key) + " (" + key.getKeyID() + ")";
        return requestPassword(msg);
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



