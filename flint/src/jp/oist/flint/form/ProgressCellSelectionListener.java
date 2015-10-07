/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import jp.oist.flint.form.sub.SubFrame;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;

public class ProgressCellSelectionListener implements PropertyChangeListener {

    private final SubFrame mSubFrame;

    public ProgressCellSelectionListener(SubFrame subFrame) {
        mSubFrame = subFrame;
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        String propertyName = evt.getPropertyName();
        if ("selected".equals(propertyName)) {
            boolean isSelected = (Boolean)evt.getNewValue();
            if (!isSelected)
                return;
            try {
                if (mSubFrame.isIcon())
                    mSubFrame.setIcon(false);
                mSubFrame.setSelected(true);
            } catch (PropertyVetoException ex) {
                // ignored
            }
        }
    }

}
