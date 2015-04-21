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
public class FileChooser {

    enum Style {
        AWT,
        SWING
    }

    static private Style mStyle;

    static {
        String osName = System.getProperty("os.name");
        if (osName != null && osName.startsWith("Mac OS X")) {
            mStyle = Style.AWT;
        } else {
            mStyle = Style.SWING;
        }
    }

    public enum Mode {
        LOAD,
        SAVE
    }

    private Component mComponent;
    private FileDialog mFileDialog;
    private JFileChooser mJFileChooser;

    public FileChooser(Component component, String title, Mode mode, String currentDirectoryPath) {
        mComponent = component;
        switch (mStyle) {
        case AWT:
            mJFileChooser = null;
            int m = (mode == Mode.LOAD) ? FileDialog.LOAD : FileDialog.SAVE;
            while (true) {
                if (component instanceof Dialog) {
                    mFileDialog = new FileDialog((Dialog)component, title, m);
                    break;
                }
                if (component instanceof Frame) {
                    mFileDialog = new FileDialog((Frame)component, title, m);
                    break;
                }
                component = component.getParent();
            }
            mFileDialog.setDirectory(currentDirectoryPath);
            break;
        case SWING:
            mFileDialog = null;
            mJFileChooser = new JFileChooser(currentDirectoryPath);
            mJFileChooser.setDialogTitle(title);
            mJFileChooser.setDialogType(mode == Mode.LOAD ? JFileChooser.OPEN_DIALOG : JFileChooser.SAVE_DIALOG);
            break;
        }
    }

    public FileChooser(Component component, String title, Mode mode, File candidate) {
        this(component, title, mode, candidate.getParent());
        switch (mStyle) {
        case AWT:
            mFileDialog.setFile(candidate.getName());
            break;
        case SWING:
            mJFileChooser.setSelectedFile(candidate);
            break;
        }
    }

    /**
     * make internal JFileChooser visible for hacking it such as filter
     */
    public JFileChooser getJFileChooser() {
        return mJFileChooser;
    }

    public boolean showDialog() {
        if (mStyle == Style.AWT) {
            mFileDialog.setVisible(true); // modal, so blocking until user choose a file or cancel
            String fs = mFileDialog.getFile();
            if (fs == null) return false;
            return true;
        } else if (mJFileChooser.getDialogType() == JFileChooser.OPEN_DIALOG) {
            return mJFileChooser.showOpenDialog(mComponent) == JFileChooser.APPROVE_OPTION;
        } else {
            return mJFileChooser.showSaveDialog(mComponent) == JFileChooser.APPROVE_OPTION;
        }
    }

    public File getSelectedFile() {
        if (mStyle == Style.AWT) {
            return new File(mFileDialog.getDirectory(), mFileDialog.getFile());
        } else {
            return mJFileChooser.getSelectedFile();
        }
    }
}
