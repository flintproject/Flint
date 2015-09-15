/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.desktop;

import jp.oist.flint.executor.PhspSimulator;
import java.util.EventListener;

public interface ISimulationListener extends EventListener {

    /**
     * This method may or may not be called once simulation is started.
     * @param simulator
     */
    void simulationStarted(PhspSimulator simulator);

    /**
     * This method is called once simulation is done.
     */
    void simulationDone();

    /**
     * This method is called once simulation pauses.
     */
    void simulationPaused();

    /**
     * This method is called once simulation resumes.
     */
    void simulationResumed();

}
