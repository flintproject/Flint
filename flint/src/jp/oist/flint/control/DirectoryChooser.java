/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.control;

import java.awt.Component;
import java.awt.Dialog;
import java.awt.FileDialog;
import java.awt.Frame;
import java.io.File;
import javax.swing.JFileChooser;

/**
 * This class is a workaround for Mac OS X's poor Swing JFileChooser.
 * Only on OS X this uses AWT FileDialog which has a native flavor
 * rather than JFileChooser :|
 * See
 * http://developer.apple.com/library/mac/#documentation/Java/Conceptual/Java14Development/07-NativePlatformIntegration/NativePlatformIntegration.html
 * http://netbeans.org/bugzilla/show_bug.cgi?id=82821
 */
public class DirectoryChooser {

    enum Style {
        AWT,
        SWING
    }

    static private final Style mStyle;

    static {
        String osName = System.getProperty("os.name");
        if (osName != null && osName.startsWith("Mac OS X")) {
            mStyle = Style.AWT;
        } else {
            mStyle = Style.SWING;
        }
    }

    private Component mComponent;
    private FileDialog mFileDialog;
    private JFileChooser mJFileChooser;

    private File mSelectedDirectory = null;

    public DirectoryChooser(Component component, String title, String defaultDirectory) {
        mComponent = component;
        switch (mStyle) {
        case AWT:
            mJFileChooser = null;
            System.setProperty("apple.awt.fileDialogForDirectories", "true");
            while (true) {
                if (component instanceof Dialog) {
                    mFileDialog = new FileDialog((Dialog)component, title);
                    break;
                }
                if (component instanceof Frame) {
                    mFileDialog = new FileDialog((Frame)component, title);
                    break;
                }
                component = component.getParent();
            }
            mFileDialog.setDirectory(defaultDirectory);
            break;
        case SWING:
            mFileDialog = null;
            mJFileChooser = new JFileChooser(defaultDirectory);
            mJFileChooser.setDialogTitle(title);
            mJFileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            break;
        }
    }

    public boolean showDialog() {
        if (mStyle == Style.AWT) {
            mFileDialog.setVisible(true); // modal, so blocking until user choose a directory or cancel
            mSelectedDirectory = new File(mFileDialog.getDirectory(), mFileDialog.getFile());
            System.setProperty("apple.awt.fileDialogForDirectories", "false");
            return mSelectedDirectory != null;
        } else {
            if (mJFileChooser.showOpenDialog(mComponent) == JFileChooser.APPROVE_OPTION) {
                mSelectedDirectory = mJFileChooser.getSelectedFile();
                return true;
            }
            return false;
        }
    }

    public File getSelectedDirectory() {
        return mSelectedDirectory;
    }
}
