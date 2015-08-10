/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.sub;

import java.awt.Component;
import java.io.File;
import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;

public class ConfirmDialogForOverwritingFile {

    private final Component mComponent;

    private final JCheckBox mDontShowAgain;

    private int mLastChosenOption;

    public ConfirmDialogForOverwritingFile(Component component) {
        mComponent = component;
        mDontShowAgain = new JCheckBox("Do not show this message again.", false);
    }

    public int show(File file) {
        if (mDontShowAgain.isSelected())
            return mLastChosenOption;

        JLabel msg1 = new JLabel(file.toPath() + " already exists.");
        JLabel msg2 = new JLabel("Do you want to overwrite it?");

        msg1.setBorder(BorderFactory.createEmptyBorder(0, 5, 0, 0));
        msg2.setBorder(BorderFactory.createEmptyBorder(0, 5, 20, 0));

        String title = "File exists";

        JOptionPane optionPane = new JOptionPane(new Object[] {msg1, msg2, mDontShowAgain},
                                                 JOptionPane.QUESTION_MESSAGE, JOptionPane.YES_NO_CANCEL_OPTION);

        JDialog dialog = optionPane.createDialog(mComponent, title);
        dialog.setModalityType(JDialog.ModalityType.APPLICATION_MODAL);
        dialog.setVisible(true);

        mLastChosenOption = (Integer)optionPane.getValue();
        return mLastChosenOption;
    }
}
