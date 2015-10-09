/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import java.awt.Component;
import javax.swing.JList;
import javax.swing.ListCellRenderer;

class ProgressListCellRenderer implements ListCellRenderer {

    @Override
    public Component getListCellRendererComponent(JList list, Object value,
                                                  int index, boolean isSelected, boolean hasFocus) {
        ProgressCell cellPane = (ProgressCell)value;

        cellPane.setEnabled(list.isEnabled());
        cellPane.setFont(list.getFont());

        if (isSelected) {
            cellPane.setBackground(list.getSelectionBackground());
            cellPane.setForeground(list.getSelectionForeground());
        } else {
            cellPane.setBackground(list.getBackground());
            cellPane.setForeground(list.getForeground());
        }

        cellPane.setOpaque(true);
        cellPane.setSelected(isSelected);
        return cellPane;
    }

}
