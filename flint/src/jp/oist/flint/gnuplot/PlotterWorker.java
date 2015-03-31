/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.gnuplot;

import jp.oist.flint.isdf.IsdfException;
import java.io.IOException;
import javax.swing.SwingWorker;
import jp.oist.flint.form.sub.IChartController;

public class PlotterWorker extends SwingWorker<Boolean, Void> {
    final Plotter mPlotter;
    final IChartController mController;

    public PlotterWorker(final Plotter plotter, final IChartController controller) {
        this.mPlotter = plotter;
        this.mController = controller;
    }

    @Override
    protected Boolean doInBackground() throws IOException, IsdfException, Exception {
        mPlotter.plot(mController);
        return Boolean.TRUE;
    }

    @Override
    protected void done() {
    }
}
