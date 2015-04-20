/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.plot.gnuplot;

import org.apache.log4j.Logger;
import java.io.BufferedReader;
import java.io.File;
import jp.oist.flint.plot.BasePlotter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

/**
 * The class of an external plotter (GnuPlotter)
 */
public class GnuPlotter extends BasePlotter {

    /** The enum for the plot type of a graph. */
    public static enum PlotType {

        TWO_DIM, THREE_DIM
    }
    /** The enum for the Terminal type of a graph. */
    public static enum TerminalType {

        WINDOW, PDF, PNG, EPS
    }
    /** The enum for the separator classification of a data file. */
    public static enum DataFileSeparator {

        TAB, COMMA
    }
    /** The extension of the config file passed to gnuplot. */
    public final static String PLOT_FILE_EXTENSION = "plt";
    /** The list which stored the data displayed on a graph. */
    private ArrayList<Gnuplot> dataSets;
    /** The list which stores the label displayed on each axis. */
    private ArrayList<GPLabel> labels;
    /** The list which stored the property class. */
    private ArrayList<GPVariable> variables;
    /** graph title. */
    private String title;
    /** external output file path. */
    private String outputFile = "";
    /** The label of an x-axis. */
    private String xlabel;
    /** The label of an y-axis. */
    private String ylabel;
    /** The label of an y2-axis. */
    private String y2label;
    /** The minimum of an x-axis. */
    private Double xmin;
    /** The maximum of an x-axis. */
    private Double xmax;
    /** The minimum of an y-axis. */
    private Double ymin;
    /** The maximum of an y-axis. */
    private Double ymax;
    /** The minimum of an y2-axis. */
    private Double y2min;
    /** The maximum of an y2-axis. */
    private Double y2max;
    /** The display flag of logscale of an x-axis. */
    private boolean logScaleX = false;
    /** The display flag of logscale of an y-axis. */
    private boolean logScaleY = false;
    /** The display flag of logscale of an y2-axis. */
    private boolean logScaleY2 = false;
    /** The plot type of a graph. */
    private PlotType plotType;
    /** The terminal type of a graph. */
    private TerminalType terminalType;
    /** The separator classification of a data file. */
    private DataFileSeparator dataFileSeparator = DataFileSeparator.COMMA;
    /** The display of introductory notes. */
    private boolean keyDisplayFlg = false;

    private String mConfigFilePath;

    /**
     * Default Constructor.
     */
    public GnuPlotter() {
        super();
        dataSets = new ArrayList<>();
        labels = new ArrayList<>();
        variables = new ArrayList<>();
    }

    /**
     * An external output file path is returned.
     *
     * @return String
     */
    public String getOutputFile() {
        return outputFile;
    }

    /**
     * An external output file path is set.
     *
     * @param outputFile
     */
    public void setOutputFile(String outputFile) {
        this.outputFile = outputFile;
    }

    /**
     * The list which stored the data displayed on a graph is returned.
     *
     * @return ArrayList<Gnuplot>
     */
    public ArrayList<Gnuplot> getDataSets() {
        return dataSets;
    }

    /**
     * The list which stored the data displayed on a graph is set.
     *
     * @param dataSets the dataSets to set
     */
    public void setDataSets(ArrayList<Gnuplot> dataSets) {
        this.dataSets = dataSets;
    }

    /**
     * The list which stores the label displayed on each axis is returned.
     *
     * @return ArrayList<GPLabel>
     */
    public ArrayList<GPLabel> getLabels() {
        return labels;
    }

    /**
     * The list which stores the label displayed on each axis is set.
     *
     * @param labels the labels to set
     */
    public void setLabels(ArrayList<GPLabel> labels) {
        this.labels = labels;
    }

    /**
     * The list which stored the property class is returned.
     *
     * @return ArrayList<GPVariable>
     */
    public ArrayList<GPVariable> getVariables() {
        return variables;
    }

    /**
     * The list which stored the property class is set.
     *
     * @param variables the variables to set
     */
    public void setVariables(ArrayList<GPVariable> variables) {
        this.variables = variables;
    }

    /**
     * A graph title is returned.
     *
     * @return String
     */
    public String getTitle() {
        return title;
    }

    /**
     * A graph title is set.
     *
     * @param title
     */
    public void setTitle(String title) {
        this.title = title;
    }

    /**
     * The label of an x-axis is returned.
     *
     * @return String
     */
    public String getXlabel() {
        return xlabel;
    }

    /**
     * The label of an x-axis is set.
     *
     * @param xlabel
     */
    public void setXlabel(String xlabel) {
        this.xlabel = xlabel;
    }

    /**
     * The label of an y-axis is returned.
     *
     * @return the ylabel
     */
    public String getYlabel() {
        return ylabel;
    }

    /**
     * The label of an y-axis is set.
     *
     * @param ylabel the ylabel to set
     */
    public void setYlabel(String ylabel) {
        this.ylabel = ylabel;
    }

    /**
     * The label of an y3-axis is returned.
     *
     * @return the y2label
     */
    public String getY2label() {
        return y2label;
    }

    /**
     * The label of an y2-axis is set.
     *
     * @param y2label the y2label to set
     */
    public void setY2label(String y2label) {
        this.y2label = y2label;
    }

    /**
     * The minimum of an x-axis is returned.
     *
     * @return Double
     */
    public Double getXmin() {
        return xmin;
    }

    /**
     * The minimum of an x-axis is set.
     *
     * @param xmin
     */
    public void setXmin(Double xmin) {
        this.xmin = xmin;
    }

    /**
     * The maximum of an x-axis is returned.
     *
     * @return Double
     */
    public Double getXmax() {
        return xmax;
    }

    /**
     * The maximum of an x-axis is set.
     *
     * @param xmax
     */
    public void setXmax(Double xmax) {
        this.xmax = xmax;
    }

    /**
     * The minimum of an y-axis is returned.
     *
     * @return Double
     */
    public Double getYmin() {
        return ymin;
    }

    /**
     * The minimum of an y-axis is set.
     *
     * @param ymin
     */
    public void setYmin(Double ymin) {
        this.ymin = ymin;
    }

    /**
     * The maximum of an x-axis is returned.
     *
     * @return Double
     */
    public Double getYmax() {
        return ymax;
    }

    /**
     * The maximum of an y-axis is set.
     *
     * @param ymax
     */
    public void setYmax(Double ymax) {
        this.ymax = ymax;
    }

    /**
     * The minimum of an y2-axis is returned.
     *
     * @return Double
     */
    public Double getY2min() {
        return y2min;
    }

    /**
     * The minimum of an y2-axis is set.
     *
     * @param y2min
     */
    public void setY2min(Double y2min) {
        this.y2min = y2min;
    }

    /**
     * The maximum of an y2-axis is returned.
     *
     * @return Double
     */
    public Double getY2max() {
        return y2max;
    }

    /**
     * The maximum of an y2-axis is set.
     *
     * @param y2max
     */
    public void setY2max(Double y2max) {
        this.y2max = y2max;
    }

    /**
     * The display flag of logscale of an x-axis is returned.
     *
     * @return boolean
     */
    public boolean isLogScaleX() {
        return logScaleX;
    }

    /**
     * The display flag of logscale of an x-axis is set.
     *
     * @param logScaleX
     */
    public void setLogScaleX(boolean logScaleX) {
        this.logScaleX = logScaleX;
    }

    /**
     * The display flag of logscale of an y-axis is returned.
     *
     * @return boolean
     */
    public boolean isLogScaleY() {
        return logScaleY;
    }

    /**
     * The display flag of logscale of an y-axis is set.
     *
     * @param logScaleY
     */
    public void setLogScaleY(boolean logScaleY) {
        this.logScaleY = logScaleY;
    }

    /**
     * The display flag of logscale of an y2-axis is returned.
     *
     * @return boolean
     */
    public boolean isLogScaleY2() {
        return logScaleY2;
    }

    /**
     * The display flag of logscale of an y2-axis is set.
     *
     * @param logScaleY2
     */
    public void setLogScaleY2(boolean logScaleY2) {
        this.logScaleY2 = logScaleY2;
    }

    /**
     * The plot type of a graph is returned.
     *
     * @return PlotType
     */
    public PlotType getPlotType() {
        return plotType;
    }

    /**
     * The plot type of a graph is set.
     *
     * @param plotType
     */
    public void setPlotType(PlotType plotType) {
        this.plotType = plotType;
    }

    /**
     * The Terminal type of a graph is returned.
     *
     * @return TerminalType
     */
    public TerminalType getTerminalType() {
        return terminalType;
    }

    /**
     * The Terminal type of a graph is set.
     *
     * @param terminalType
     */
    public void setTerminalType(TerminalType terminalType) {
        this.terminalType = terminalType;
    }

    /**
     * The separator classification of a data file is returned.
     *
     * @return DataFileSeparator
     */
    public DataFileSeparator getDataFileSeparator() {
        return dataFileSeparator;
    }

    /**
     * The separator classification of a data file is set.
     *
     * @param dataFileSeparator
     */
    public void setDataFileSeparator(DataFileSeparator dataFileSeparator) {
        this.dataFileSeparator = dataFileSeparator;
    }

    /**
     * The display flag of introductory notes is returned.
     *
     * @return boolean
     */
    public boolean isKeyDisplayFlg() {
        return keyDisplayFlg;
    }
    /**
     * The display flag of introductory notes is set.
     *
     * @param _keyDisplayFlg
     */
    public void setKeyDisplayFlg(boolean _keyDisplayFlg) {
        this.keyDisplayFlg = _keyDisplayFlg;
    }

    public void createConfigFile() throws IOException {
        mConfigFilePath = getDataFilePath() + "." + PLOT_FILE_EXTENSION;
            File outputFile = new File(mConfigFilePath);
            outputFile.deleteOnExit();
        try (FileWriter configWriter = new FileWriter(outputFile, false)) {
            configWriter.write(getPlotString());
        }
    }

    /**
     * Execution of Exterior Plotter.
     */
    @Override
    public void plotting() throws Exception {
        List<String> list = new ArrayList<>();
        list.add(getPlotterPath());
        list.add("-persist");
        list.add(mConfigFilePath);

        ProcessBuilder pb = new ProcessBuilder(list);
        pb.redirectErrorStream(true);

        boolean result = submit(pb);

        String separator = System.getProperty("line.separator");

        BufferedReader reader = new BufferedReader(new InputStreamReader(getProcess().getInputStream()));

        StringBuilder sb = new StringBuilder();
        String line;
        while ((line=reader.readLine()) != null)
            sb.append(line).append(separator);

        if (!result) {
            Logger.getRootLogger().error(sb.toString());
            throw new Exception("Error while executing the plotter.");
        }
    }

    /**
     * The information written in the configuration file of Exterior Plotter is returned.
     *
     * @return String
     */
    public String getPlotString() {
        String s = "";

        switch (getTerminalType()) {
        case PDF:
        case PNG:
        case EPS:
            s += "set terminal " + getTerminalType().name().toLowerCase() + "\n";
            break;
        default:
            String osName = System.getProperty("os.name");
            if (osName != null && osName.startsWith("Windows")) {
                s += "set terminal windows\n";
            } else {
                s += "set terminal X11\n";
            }
            break;
        }

        if (!getTerminalType().equals(TerminalType.WINDOW)) {
            s += "set output \""+ outputFile + "\" \n";
        }

        for (int i = 0; i < getVariables().size(); i++) {
            if (getVariables().get(i).getType().equals(GPVariable.Type.GNUPLOT)) {
                if (getVariables().get(i).isActive()) {
                    s += ((GPGnuplotVariable) getVariables().get(i)).getPlotString() + "\n";
                }
            }
        }

        if (dataFileSeparator == DataFileSeparator.COMMA) {
            s += "set datafile separator \",\"\n";
        } else {
            s += "set datafile separator \"\t\"\n";
        }

        if (getTitle() == null) {
            s += "unset title\n";
        } else {
            s += "set title \"" + getTitle() + "\"\n";
        }

        if (getXlabel() == null) {
            s += "unset xlabel\n";
        } else {
            s += "set xlabel \"" + getXlabel() + "\"\n";
        }

        if (getYlabel() == null) {
            s += "unset ylabel\n";
        } else {
            s += "set ylabel \"" + getYlabel() + "\"\n";
        }

        if (getY2label() == null) {
            s += "unset y2label\n";
        } else {
            s += "set y2label \"" + getY2label() + "\"\n";
        }

        if (isLogScaleX()) {
            s += "set logscale x\n";
        } else {
            s += "unset logscale x\n";
        }

        if (isLogScaleY()) {
            s += "set logscale y\n";
        } else {
            s += "unset logscale y\n";
        }

        if (isLogScaleY2()) {
            s += "set logscale y2\n";
        } else {
            s += "unset logscale y2\n";
        }

        if (isKeyDisplayFlg()) {
            s += "set key box\n";
        } else {
            s += "unset key\n";
        }

        for (int i = 0; i < getLabels().size(); i++) {
            if (getLabels().get(i).getDoPlot()) {
                s += getLabels().get(i).getPlotString();
                s += "\n";
            }
        }

        if (getXmin() != null && getXmax() != null) {
            s += "set xrange [" + getXmin() + ":" + getXmax() + "]\n";
        }

        if (getYmin() != null && getYmax() != null) {
            s += "set yrange [" + getYmin() + ":" + getYmax() + "]\n";
        }

        if (plotType == PlotType.TWO_DIM) {
            if (getY2min() != null && getY2max() != null) {
                s += "set y2range [" + getY2min() + ":" + getY2max() + "]\n";
                s += "set y2tics autofreq\n";
                s += "set ytics nomirror\n";
            }
        }

        if (plotType == PlotType.TWO_DIM) {
            s += "plot ";
        } else {
            s += "splot ";
        }

        for (int i = 0; i < getDataSets().size(); i++) {
            if (getDataSets().get(i).getDoPlot()) {
                s += getDataSets().get(i).getPlotString();
                s += ", ";
            }
        }

        s = s.trim();
        if (s.lastIndexOf(",") == s.length() - 1) {
            s = s.substring(0, s.length() - 1);
        }
        s += "\n";

        for (int i = 0; i < getVariables().size(); i++) {
            if (getVariables().get(i).getType() == GPVariable.Type.STRING) {
                if (getVariables().get(i).isActive()) {
                    s = ((GPStringVariable) getVariables().get(i)).apply(s);
                }
            }
        }

        return s;
    }
}
