/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form;

import java.awt.Container;
import java.io.File;

public interface IModelContainer {

    File getModelFile ();

    String getModelPath ();

    String getRelativeModelPath ();

	Container getContainer ();

    void notifyK3Enabled ();

    boolean outputWindowIsSelected ();

    boolean tryLock ();

    void unlock ();

}
