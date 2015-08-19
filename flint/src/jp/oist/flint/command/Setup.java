/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.command;

import jp.oist.flint.component.Component;
import jp.oist.flint.component.Linux;
import jp.oist.flint.component.MacOsX;
import jp.oist.flint.component.Windows;
import jp.oist.flint.headless.Oneshot;
import org.apache.log4j.Logger;
import java.io.File;
import java.io.IOException;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

public class Setup implements Runnable {

    private static final String REFERENCE_PROGRAM_NAME = "flint-exec";

    final CommandLineArguments mCommandLineArgs;

    private Runnable mLauncher = null;

    public Setup(final CommandLineArguments commandLineArgs) {
        mCommandLineArgs = commandLineArgs;
    }

    private File setupComponent() {
        JFileChooser fc = new JFileChooser();
        fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

        // check previous choice
        Preferences prefs = Preferences.userRoot().node("/jp/oist/flint");
        String previous = prefs.get("defaultComponentPath", "");
        File previousFile = new File(previous);
        if ( previousFile.isDirectory() &&
             new File(previousFile, REFERENCE_PROGRAM_NAME).isFile() ) {
            // do not ask user if available
            return previousFile;
        }
        // othewise, user has to specify its path
        fc.setDialogTitle("Specify component directory");
        if (fc.showOpenDialog(null) != JFileChooser.APPROVE_OPTION) {
            Logger.getRootLogger().fatal("could not find components");
            System.exit(1); // never return
        }
        File file = fc.getSelectedFile();
        try {
            String path = file.getCanonicalPath();
            prefs.put("defaultComponentPath", path);
            prefs.flush();
        } catch (BackingStoreException bse) {
            Logger.getRootLogger().error(bse.getMessage());
            // print the exception, but just go next
        } catch (IOException ioe) {
            Logger.getRootLogger().fatal(ioe.getMessage());
            System.exit(1); // never return
        }
        return file;
    }

    @Override
    public void run() {
        String osName = System.getProperty("os.name");
        if (osName == null) {
            Logger.getRootLogger().fatal("could not detect OS name");
            System.exit(1); // never return
        }

        if (osName.startsWith("Windows")) {
            Windows windows = new Windows();
            Component.setOs(windows);
        } else if (osName.startsWith("Mac OS X")) {
            MacOsX macOsX = new MacOsX();
            Component.setOs(macOsX);
        } else { // Linux
            Linux linux = new Linux();
            Component.setOs(linux);

            // try to find a command executable in order to check the component directory
            String userDir = System.getProperty("user.dir");
            File binDir = new File(userDir, "bin");
            if ( !binDir.isDirectory() ||
                 !new File(binDir, REFERENCE_PROGRAM_NAME).isFile() ) {
                File dir = setupComponent();
                Component.setPath(dir);
            }
        }

        if (mCommandLineArgs.isHeadless()) {
            mLauncher = new Oneshot(mCommandLineArgs.getHeadlessOption(), true);
        } else {
            mLauncher = new Launch(mCommandLineArgs);
        }
    }

    public Runnable getLauncher () {
        return mLauncher;
    }
}
