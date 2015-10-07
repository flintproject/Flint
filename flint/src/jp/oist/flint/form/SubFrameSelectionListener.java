/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.form.sub.SubFrame;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

public class SubFrameSelectionListener implements PropertyChangeListener {

    private final ProgressPane mProgressPane;

    public SubFrameSelectionListener(ProgressPane progressPane) {
        mProgressPane = progressPane;
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        String propertyName = evt.getPropertyName();
        if ("selected".equals(propertyName)) {
            boolean isSelected = (Boolean)evt.getNewValue();
            if (!isSelected)
                return;
            SubFrame subFrame = (SubFrame)evt.getSource();
            ProgressCell cell = (ProgressCell)subFrame.getStatusComponent();
            if (cell == null)
                return;
            mProgressPane.setSelectedCell(cell, true);
        }
    }
}
