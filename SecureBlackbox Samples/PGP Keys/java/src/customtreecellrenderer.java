import java.awt.Component;
import java.net.URL;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;

import secureblackbox.*;

public class customtreecellrenderer extends DefaultTreeCellRenderer
{
	private static final long serialVersionUID = 1L;
	
    /** 
    * getTreeCellRendererComponent 
    * This method is overridden to set the node specific icons and tooltips
    *    
    * @return The Component object used to render the cell value
    * @exception 
    */ 
    public Component getTreeCellRendererComponent(JTree tree, Object value,
                                                  boolean selection, boolean expanded,
                                                  boolean leaf, int row, boolean hasFocus)
    {
        super.getTreeCellRendererComponent(tree, value, selection, expanded, 
                                           leaf, row, hasFocus);
        
        //The value object is nothing but the DefaultMutableTreeNode.
        DefaultMutableTreeNode node = (DefaultMutableTreeNode)value;
        
        setIconAndToolTip(node.getUserObject(), tree);
        
        return this;
    }
    
	private void setIconAndToolTip(Object obj, JTree tree)
    {
    	if (obj instanceof nodedata)
    	{
			nodedata node = (nodedata) obj;

			setFont(node.getFont());

			Object tag = node.getTag();
			PGPKey key = (PGPKey) tag;

			if (key.getPublicKeyAlgorithm().contains("RSA"))
			{
				setIcon(getIcon("dkey"));
			}
			else
			{
				setIcon(getIcon("ekey"));
			}
		}
    }
	
	private Icon getIcon(String name) {
		URL resPath = getClass().getResource("/" + name + ".ico");
		if (resPath == null) return new ImageIcon();
		return new ImageIcon(resPath);
	}

}