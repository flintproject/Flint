/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.plotter;

import javax.swing.JDialog;
import javax.swing.SwingWorker;
import jp.oist.flint.form.sub.IChartController;

public interface IPlotter {
    SwingWorker getWorker(final IChartController controller);
    void preference(final JDialog parent);
    @Override
    String toString();
}
