/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.desktop;

import jp.oist.flint.component.Component;
import java.io.File;
import java.io.IOException;
import java.util.Scanner;
import javax.swing.SwingWorker;

public class PauseWorker extends SwingWorker<Boolean, Void> {

    private final File mPidFile;

    public PauseWorker(File pidFile) {
        mPidFile = pidFile;
    }

    @Override
    protected Boolean doInBackground() throws IOException, InterruptedException {
        try (Scanner s = new Scanner(mPidFile, "UTF8")) {
            int pid = s.nextInt();
            if (pid <= 0)
                return false;
            ProcessBuilder builder = new ProcessBuilder(Component.getFlintPauseCommand(pid));
            Component.setUpEnvironment(builder);
            Process process = builder.start();
            return process.waitFor() == 0;
        }
    }
}
