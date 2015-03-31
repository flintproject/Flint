/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.plot;

import java.io.IOException;

/**
 * This is the base class of external plotter (deprecated)
 */
public abstract class BasePlotter {

    /** The path of Exterior Plotter. */
    private String plotterPath;
    /** Path of the data file passed to Exterior Plotter. */
    private String dataFilePath;

    private Process mProcess;

    public abstract void plotting() throws Exception;

    /**
     * Execution of Exterior Plotter.
     *
     * @param pb The execution process of Exterior Plotter.
     * @return true if the result is suceess. otherwise false.
     */
    protected boolean submit (final ProcessBuilder pb) 
            throws IOException, InterruptedException {

        mProcess = pb.start();

        int result = mProcess.waitFor();
        return (result == 0);
    }

    public Process getProcess () {
        return mProcess;
    }

    /**
     * Returns The path of Exterior Plotter.
     *
     * @return String The path of Exterior Plotter.
     */
    public String getPlotterPath() {
        return plotterPath;
    }

    /**
     * Path of Exterior Plotter is set.
     *
     * @param plotterPath Path of Exterior Plotter.
     */
    public void setPlotterPath(String plotterPath) {
        this.plotterPath = plotterPath;
    }

    /**
     * Returns The data file Path passed to Exterior Plotter.
     *
     * @return String The data file Path passed to Exterior Plotter.
     */
    public String getDataFilePath() {
        return dataFilePath;
    }

    /**
     * Path of the data file passed to Exterior Plotter is set.
     *
     * @param dataFilePath Path of the data file passed to Exterior Plotter.
     */
    public void setDataFilePath(String dataFilePath) {
        this.dataFilePath = dataFilePath;
    }
}
