import java.awt.Font;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;

import secureblackbox.*;

public class utils {

	public static void RedrawKeyring(JTree tree, Pgpkeyring keyring, DefaultMutableTreeNode rootNode) {
		JTree tv = tree;
		int i, j;
		customtreecellrenderer render = new customtreecellrenderer();
		tree.setCellRenderer(render);
		DefaultMutableTreeNode KeyNode, SubKeyNode;
		Font NodeFont = tree.getFont();
		String nodeTitle = "";
		
		rootNode.removeAllChildren();
		for(i = 0; i < keyring.getPublicKeys().size(); i++)
		{
			if (!keyring.getPublicKeys().item(i).getIsSubkey())
			{
				nodeTitle = GetDefaultUserID(keyring.getPublicKeys().item(i));
				KeyNode = new DefaultMutableTreeNode(nodeTitle);
				rootNode.add(KeyNode);

				if (keyring.getPublicKeys().item(i).getIsSecret() && keyring.getPublicKeys().item(i).getRevoked()) {
					NodeFont = new Font(tv.getFont().getFontName(), Font.BOLD + Font.ITALIC, 10);
				} else if (keyring.getPublicKeys().item(i).getIsSecret()) {
					NodeFont = new Font(tv.getFont().getFontName(), Font.BOLD, 10);
				} else if (keyring.getPublicKeys().item(i).getRevoked()) {
					NodeFont = new Font(tv.getFont().getFontName(), Font.ITALIC, 10);
				}

				/* Creating key node */
				KeyNode.setUserObject(new nodedata(keyring.getPublicKeys().item(i), nodeTitle, NodeFont));

				/* Sub keys */
				for (j = 0; j < keyring.getPublicKeys().size(); j++)
				{
					if (keyring.getPublicKeys().item(j).getIsSubkey() && keyring.getPublicKeys().item(j).getPrimaryKeyID().equalsIgnoreCase(keyring.getPublicKeys().item(i).getKeyID()))
					{
						nodeTitle = keyring.getPublicKeys().item(j).getPublicKeyAlgorithm() + " subkey";
						SubKeyNode = new DefaultMutableTreeNode(nodeTitle);
						KeyNode.add(SubKeyNode);
						SubKeyNode.setUserObject(new nodedata(keyring.getPublicKeys().item(j), nodeTitle, tree.getFont()));
					}
				}
			}
		}	
		refreshTree(tree);
	}

	private static void refreshTree(JTree tree) {
		DefaultTreeModel model = ((DefaultTreeModel)tree.getModel());
		model.reload();
		expandTree(tree);
	}

	private static void expandTree(JTree tree) {	
	    int row = 0;
	    while (row < tree.getRowCount()) {
	      tree.expandRow(row);
	      row++;
	    }
	}
	
	public static String GetDefaultUserID(PGPKey key)
	{
		String result;
		if (key.getUsername() != "")
		{
			result = key.getUsername();
		} 
		else 
		{
			result = "No name";
		}
		return result;
	}
}
