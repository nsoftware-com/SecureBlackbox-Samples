import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JPopupMenu;
import javax.swing.JTable;

class tablepopuplistener extends MouseAdapter {
    JPopupMenu popup;  
  
    JTable table;  
    public tablepopuplistener(JPopupMenu popup, JTable table) {
           this.popup=popup;  
           this.table=table;  
    }  
  
   public void mousePressed(MouseEvent me)  
   {  
    firePopup(me);  
   }  
   public void mouseReleased(MouseEvent me)  
   {  
    firePopup(me);  
   }  
  
  
   public void firePopup(MouseEvent me)  
   {  
      
    /* 
     * The pop up menu will be shown only if there is a row selection in the table 
     */  
    if(me.isPopupTrigger()&&table.getModel().getRowCount()!=0&&  
       table.getSelectedRow()!=-1)  
    {  
        popup.show(table,me.getX(),me.getY());  
    }  
   }  
}  