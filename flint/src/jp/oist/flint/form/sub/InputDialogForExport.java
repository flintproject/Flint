/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.sub;

import java.awt.Component;
import java.util.Locale;
import javax.swing.JOptionPane;

public class InputDialogForExport {

    private final Component mComponent;

    public InputDialogForExport(Component component) {
        mComponent = component;
    }

    public String show() {
        Object[] possibleValues = {"CSV", "ISD"};
        String result = (String)JOptionPane.showInputDialog(mComponent,
                                                            "Which file format do you want to export in?",
                                                            "Choose the file format",
                                                            JOptionPane.QUESTION_MESSAGE,
                                                            null,
                                                            possibleValues,
                                                            possibleValues[0]);
        if (result == null)
            return null;
        return result.toLowerCase(Locale.ENGLISH);
    }
}
