/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.sedml;

import jp.physiome.Ipc;
import java.util.ArrayList;

public interface ISimulationConfiguration {

    /* should return a path obtained by File#getCanonicalPath() */
    String getModelCanonicalPath();

    Ipc.IntegrationMethod getIntegrationMethod();

    String getLength();

    String getStep();

    int getGranularity();

    String getOutputStartTime();

    ArrayList<String> getKeys();

    int getFilterSyntax();

    String getFilterPattern();

    int getFilterColumn();

}
