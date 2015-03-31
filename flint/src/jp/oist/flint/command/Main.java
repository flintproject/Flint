/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.command;

import org.apache.log4j.Logger;
import java.lang.reflect.InvocationTargetException;
import javax.swing.SwingUtilities;
import jp.oist.flint.form.MainFrame;

public class Main {

    /**
     * @param args the command line arguments
     */
    public static void main(final String args[]) {
        try {

            new Main(args);

        } catch (InvocationTargetException | InterruptedException ex) {
            Logger.getRootLogger().fatal(ex.getMessage());
            System.exit(1);
        }
    }

    private final MainFrame mMainFrame;

    public Main (String[] args) throws InvocationTargetException, InterruptedException {
        CommandLineArguments commandLineArgs = new CommandLineArguments();
        commandLineArgs.parse(args);

        Setup setup = new Setup(commandLineArgs);

        SwingUtilities.invokeAndWait(setup);

        Runnable launcher = setup.getLauncher();

        SwingUtilities.invokeAndWait(launcher);

        if (launcher instanceof Launch) {
            mMainFrame = ((Launch)launcher).getMainFrame();
        } else {
            mMainFrame = null;
        }
    }
        

    public MainFrame getMainFrame () {
        return mMainFrame;
    }
}
