/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.headless;

import jp.oist.flint.executor.SimulatorService;
import jp.physiome.Cli.RunOption;
import java.io.File;
import java.util.concurrent.ExecutionException;

public class Oneshot implements Runnable {

    final RunOption mOption;

    final boolean mQuit;

    public Oneshot(RunOption option, boolean quit) {
        mOption = option;
        mQuit = quit;
    }

    @Override
    public void run() {
        final Logger logger = new Logger();
        File inputFile = new File(mOption.getModelFilename());

        if (!inputFile.exists()) {
            logger.printError("could not find " + mOption.getModelFilename());
            if (mQuit) System.exit(1);
            return;
        }

        if (mOption.hasSpecFilename()) {
            File specFile = new File(mOption.getSpecFilename());
            if (!specFile.isFile()) {
                logger.printError("could not find " + specFile.getPath());
                if (mQuit) System.exit(1);
                return;
            }
        }

        SimulatorService service = new SimulatorService(logger);
        final Simulator simulator = new Simulator(service, mOption);
        simulator.execute();
        try {
            if (mQuit)
                System.exit(simulator.get() ? 0 : 1);
        } catch (InterruptedException | ExecutionException ex) {
            logger.printError(ex.getMessage());
            if (mQuit)
                System.exit(1);
        }
    }
}
