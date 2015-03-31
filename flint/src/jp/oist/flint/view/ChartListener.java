/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.view;

import jp.oist.flint.form.sub.IChartController;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;


public class ChartListener implements ListDataListener {

    private final IChartController mController;

    public ChartListener(IChartController controller) {
        mController = controller;
    }

    @Override
    public void contentsChanged(ListDataEvent listDataEvent) {
    }

    @Override
    public void intervalAdded(ListDataEvent listDataEvent) {
    }

    @Override
    public void intervalRemoved(ListDataEvent listDataEvent) {
        mController.renderPlot();
    }

}
