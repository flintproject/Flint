/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.plot.gnuplot;

/**
 * This is a class which manages the label used by gnuplotter.
 */
public class GPLabel {

    /** The enum of the display position of a label. */
    public static enum GPRelativePos {

        first,
        second,
        screen,
        graph
    }
    /** Width of the value of an x-axis. */
    private double x;
    /** Width of the value of an y-axis. */
    private double y;
    /** The axis name which displays a label. */
    private String text;
    /** The display position of a label. */
    private GPRelativePos relativePos = GPRelativePos.first;
    /** The display flag of a label . */
    private boolean doPlotFlg = true;

    /**
     * The display flag of a label is returned.
     *
     * @return doPlotFlg
     */
    public boolean getDoPlot() {
        return doPlotFlg;
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
     * The option character string passed to gnuplot is returned.
     *
     * @return  String
     */
    public String getPlotString() {
        return "set label \"" + text + "\" at " + relativePos.name() + " " + x + "," + y;
    }

    /**
     * The axis name which displays a label is returned.
     *
     * @return String
     */
    public String getText() {
        return text;
    }

    /**
     * The axis name which displays a label is set.
     *
     * @param text
     */
    public void setText(String text) {
        this.text = text;
    }

    /**
     * The width of the value of an x-axis is returned.
     *
     * @return double
     */
    public double getX() {
        return x;
    }

    /**
     * The width of the value of an x-axis is set.
     *
     * @param x
     */
    public void setX(double x) {
        this.x = x;
    }

    /**
     * The width of the value of an y-axis is returned.
     *
     * @return double
     */
    public double getY() {
        return y;
    }

    /**
     * The width of the value of an y-axis is set.
     *
     * @param y
     */
    public void setY(double y) {
        this.y = y;
    }

    /**
     * The data of a GPLabel class is returned.
     *
     * @return Object[]
     */
    public Object[] getData() {
        Object data[] = new Object[5];
        data[0] = this.text;
        data[1] = this.x;
        data[2] = this.y;
        data[3] = this.relativePos;
        data[4] = this.doPlotFlg;
        return data;
    }

    /**
     * Data is set to a GPLabel class.
     *
     * @param i
     * @param value
     */
    public void setData(int i, Object value) {
        if (i == 0) {
            text = (String) value;
        } else if (i == 1) {
            x = ((Double) value);
        } else if (i == 2) {
            y = ((Double) value);
        } else if (i == 3) {
            relativePos = (GPRelativePos) value;
        } else if (i == 4) {
            doPlotFlg = ((Boolean) value);
        }
    }

    /**
     * The display position of a label is returned.
     *
     * @return relativePos
     */
    public GPRelativePos getRelativePos() {
        return relativePos;
    }

    /**
     * The display position of a label is set.
     *
     * @param relativePos
     */
    public void setRelativePos(GPRelativePos relativePos) {
        this.relativePos = relativePos;
    }
}
