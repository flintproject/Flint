/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.util;

import java.awt.Dimension;
import java.awt.Insets;
import javax.swing.JButton;

public class ComponentFactory {

    public static JButton createButton (String s, String actionCmd, 
            Dimension fixedSize) {
        JButton btn = new JButton(s);
        btn.setActionCommand(actionCmd);
        btn.setSize(fixedSize);
        btn.setPreferredSize(fixedSize);
        btn.setMaximumSize(fixedSize);
        btn.setMinimumSize(fixedSize);
        return btn;
    }

    public static JButton createSquareButton (String s, String actionCmd,
            Dimension fixedSize) {
        JButton btn = createButton(s, actionCmd, fixedSize);
        btn.setMargin(new Insets(0, 0, 0, 0));

        return btn;
    }

    private ComponentFactory () {}
}
