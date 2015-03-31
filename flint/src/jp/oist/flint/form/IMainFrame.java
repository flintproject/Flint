/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import java.io.File;

public interface IMainFrame {

    void notifyK3Enabled();

    void notifySubJFrameAdded(IModelContainer frame);

    boolean openModel(File file);

    void setPlotterSettingTabEnabled(String defaultPlotter);

    void showErrorDialog(String message, String title);

}
