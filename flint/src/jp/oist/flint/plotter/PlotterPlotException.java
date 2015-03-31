/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.plotter;

public class PlotterPlotException extends Exception {

    public PlotterPlotException(String message) {
        super(message);
    }

    public PlotterPlotException(String message, Throwable cause) {
        super(message, cause);
    }
}
