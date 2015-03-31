/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.plot.gnuplot;

import java.awt.Color;

/**
 * The class which manages the basic information on Gnuplot.
 *
 * @author miyahira
 */
public class Gnuplot {

    /** The enum of the kind of line of a graph. */
    public static enum GPStyle {

        lines, points, linespoints, impulses,
        dots, steps, fsteps, histeps, errorbars, xerrorbars,
        yerrorbars, xyerrorbars, errorlines, xerrorlines, yerrorlines,
        xyerrorlines, boxes, filledcurves, boxerrorbars,
        boxxyerrorbars, financebars, candlesticks, vectors
    }
    /** The title of a graph. */
    private String title;
    /** The axis which displays data. */
    private String axes;
    /** The display flag of a label. */
    private boolean doPlotFlg = true;
    /** The kind of line of a graph. */
    private GPStyle style;
    /** The color of the line. */
    private GPColor color;
    /** The data information displayed on a graph from the inside of a data file. */
    private String dataString;
    /** The data file name passed to an external plotter. */
    private String fileName;
    /** The option of the kind of line of a graph. */
    private String addStyleOpt;

    private int mNumberOfColumns;
    private int mSkip;

    /**
     * The data of a Gnuplot class is returned.
     *
     * @return Object[]
     */
    public Object[] getData() {
        Object data[] = new Object[8];
        data[0] = this.fileName;
        data[1] = this.dataString;
        data[2] = this.title;
        data[3] = this.color;
        data[4] = this.style;
        if (this.addStyleOpt == null) {
            addStyleOpt = "";
        }
        data[5] = this.addStyleOpt;
        data[6] = this.doPlotFlg;
        return data;
    }

    /**
     * Data is set to a Gnuplot class.
     *
     * @param i
     * @param value
     */
    public void setData(int i, Object value) {
        if (i == 0) {
            fileName = (String) value;
        } else if (i == 1) {
            dataString = (String) value;
        } else if (i == 2) {
            title = (String) value;
        } else if (i == 3) {
            color = new GPColor((Color) value);
        } else if (i == 4) {
            style = (GPStyle) value;
        } else if (i == 5) {
            addStyleOpt = (String) value;
        } else if (i == 6) {
            doPlotFlg = ((Boolean) value).booleanValue();
        }
    }

    /**
     * Default Constructor.
     */
    public Gnuplot() {
    }

    /**
     * Constructor.
     *
     * @param fileName
     * @param dataString
     * @param title
     * @param axes
     * @param style
     */
    public Gnuplot(String fileName, String dataString,
                   int numberOfColumns, int skip,
                   String title, String axes, GPStyle style) {
        super();
        this.dataString = dataString;
        this.fileName = fileName;
        this.mNumberOfColumns = numberOfColumns;
        this.mSkip = skip;
        this.title = title;
        this.axes = axes;
        this.style = style;
    }

    /**
     * The option character string passed to gnuplot is returned.
     *
     * @return  String
     */
    public String getPlotString() {

        String s = "";

        fileName = fileName.replace("\\", "/");

        s += "\"" + fileName + "\" binary format=\"%" + mNumberOfColumns + "double\" skip=" + mSkip;
        s += " using " + dataString;

        if (axes != null && !axes.trim().isEmpty()) {
            s += " axes " + axes;
        }

        if (title != null && !title.trim().isEmpty()) {
            s += " title \"" + title + "\"";
        }

        if (style != null || addStyleOpt != null || color != null) {
            s += " with";
            if (style != null) {
                s += " " + style.name();
            }
            if (addStyleOpt != null) {
                s += " " + addStyleOpt;
            }
            if (color != null) {

                s += " lc rgb '#" + color.getHexString() + "'";
            }
        }

        return s;
    }

    /**
     * The data file name passed to a plotter is returned.
     *
     * @return String
     */
    public String getFileName() {
        return fileName;
    }

    /**
     * The data file name passed to a plotter is set.
     *
     * @param s
     */
    public void setFileName(String s) {
        this.fileName = s;
    }

    /**
     * The data information displayed on a graph from the inside of a data file is returned.
     *
     * @return String
     */
    public String getDataString() {
        return dataString;
    }

    /**
     * The data information displayed on a graph from the inside of a data file is set.
     *
     * @param function
     */
    public void setDataString(String function) {
        this.dataString = function;
    }

    /**
     * The title of a graph is returned.
     *
     * @return String
     */
    public String getTitle() {
        return title;
    }

    /**
     * The title of a graph is set.
     *
     * @param title
     */
    public void setTitle(String title) {
        this.title = title;
    }

    /**
     * The display flag of a label is returned.
     *
     * @return doPlotFlg
     */
    public boolean getDoPlot() {
        return this.doPlotFlg;
    }

    /**
     * The display flag of a label is set.
     *
     * @param doPlot
     */
    public void setDoPlot(boolean doPlot) {
        this.doPlotFlg = doPlot;
    }

    /**
     * The kind of line of a graph is returned.
     *
     * @return GPStyle
     */
    public GPStyle getStyle() {
        return this.style;
    }

    /**
     * The option of the kind of line of a graph is returned.
     *
     * @return String
     */
    public String getAddStyleOpt() {
        return addStyleOpt;
    }

    /**
     * The option of the kind of line of a graph is set.
     *
     * @param addStyleOpt
     */
    public void setAddStyleOpt(String addStyleOpt) {
        this.addStyleOpt = addStyleOpt;
    }

    /**
     * The color of the line of a graph is returned.
     *
     * @return GPColor
     */
    public GPColor getColor() {
        return color;
    }

    /**
     * The color of the line of a graph is set.
     *
     * @param color
     */
    public void setColor(GPColor color) {
        this.color = color;
    }

    /**
     * The axis which displays data is returned.
     *
     * @return axes
     */
    public String getAxes() {
        return axes;
    }

    /**
     * The axis which displays data is set.
     *
     * @param axes
     */
    public void setAxes(String axes) {
        this.axes = axes;
    }
}
