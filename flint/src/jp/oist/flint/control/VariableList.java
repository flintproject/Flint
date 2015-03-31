/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.control;

import jp.oist.flint.util.ListItem;
import jp.oist.flint.util.ListItemModel;
import java.awt.event.MouseEvent;
import javax.swing.JList;

public class VariableList extends JList {

    public VariableList(ListItemModel model) {
        super(model);
    }

    @Override
    public String getToolTipText(MouseEvent event) {
        int i = locationToIndex(event.getPoint());
        ListItem item = (ListItem)getModel().getElementAt(i);
        return item.getKey();
    }
}
