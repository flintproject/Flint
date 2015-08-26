/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import java.io.File;

public interface IMenuDelegator {
    void openPerformed(Object source);

    void recentModelPerformed (Object source, File model);

    void closePerformed (Object source);

    void loadConfigurationPerformed (Object source);

    void saveConfigurationPerformed (Object source);

    void saveAsPhspPerformed (Object source);

    void exitPerformed (Object source);

    void copyPerformed (Object source);

    void cutPerformed (Object source);

    void preferencePerformed (Object source);

    void aboutPerformed (Object source);

    void simulationRunPerformed (Object source);

    void sendToK3Performed (Object source);
}
