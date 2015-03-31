/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.view;

import jp.oist.flint.form.sub.IChartController;
import jp.oist.flint.form.sub.IChartSetting;
import jp.oist.flint.util.ListItemModel;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.SwingWorker;
import jp.oist.flint.form.IModelContainer;

public class ChartUpdater extends SwingWorker<ChartConfig, Void> {

    private final IChartController mController;

    private final IModelContainer mModelContainer;

    public ChartUpdater(IModelContainer container, IChartController controller) {
        mController = controller;
        mModelContainer = container;
    }

    @Override
    protected ChartConfig doInBackground() {
        IChartSetting setting = mController.getChartSetting();
        ListItemModel xModel = setting.getXListItemModel();
        ListItemModel yModel = setting.getYListItemModel();
        ListItemModel y2Model = setting.getY2ListItemModel();
        if ( xModel.getSize() < 1 || yModel.getSize() + y2Model.getSize() < 1 ) {
            return null;
        }

        ChartConfig config = new ChartConfig();
        String xkey = xModel.firstKey();
        config.setXindex(setting.getTrackIndex(xkey));
        config.setXlegend(setting.getLegend(xkey));
        for (int i = 0; i < yModel.getSize(); i++) {
            String key = yModel.keyAt(i);
            config.addY1legend(setting.getLegend(key));
            config.addY1indices(setting.getTrackIndex(key));
        }
        for (int i = 0; i < y2Model.getSize(); i++) {
            String key = y2Model.keyAt(i);
            config.addY2legend(setting.getLegend(key));
            config.addY2indices(setting.getTrackIndex(key));
        }
        config.setLegend(setting.isLegendChecked());
        config.setXlog(setting.isXlogChecked());
        config.setY1log(setting.isY1logChecked());
        config.setY2log(setting.isY2logChecked());
        return config;
    }

    @Override
    protected void done() {
        ChartConfig config;
        try {
            config = get();
        } catch (InterruptedException | ExecutionException ex) {
            Logger.getLogger(ChartUpdater.class.getName()).log(Level.SEVERE, null, ex);
            return;
        }

        if (config == null) {
            mController.clearChart();
        } else {
            TimeSeriesReader reader = new TimeSeriesReader(mController.getIsdFile(),
                    config.getIndices(), config.getLegends());
            reader.addPropertyChangeListener(new ChartReporter(mController, reader, config));
            reader.execute();
        }
    }
}
