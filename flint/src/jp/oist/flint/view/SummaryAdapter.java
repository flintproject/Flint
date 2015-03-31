/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.view;

import jp.oist.flint.control.VariableList;
import jp.oist.flint.form.sub.IChartController;
import jp.oist.flint.form.sub.IChartSetting;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import jp.oist.flint.form.IModelContainer;
import jp.oist.flint.util.ListItemModel;

public class SummaryAdapter extends MouseAdapter {

    private final IChartController mController;

    private final IModelContainer mModelContainer;

    private final VariableList mVariableList;

    public SummaryAdapter(IChartController controller, 
                            IModelContainer container, 
                            VariableList variableList) {
        mController = controller;
        mModelContainer = container;
        mVariableList = variableList;
    }

    @Override
    public void mouseClicked(MouseEvent e) {
        int si = mVariableList.getSelectedIndex();
        if (si < 0) return;

        ListItemModel model = (ListItemModel)mVariableList.getModel();
        String key = model.keyAt(si);
        if (key == null) return;

        IChartSetting setting = mController.getChartSetting();
        String name = model.nameAt(si);
        int i = setting.getTrackIndex(key);
        TimeSeriesReader reader = new TimeSeriesReader(mController.getIsdFile(), i, key);
        reader.addPropertyChangeListener(new SummaryReporter(mController, reader, name));
        reader.execute();
    }
}
