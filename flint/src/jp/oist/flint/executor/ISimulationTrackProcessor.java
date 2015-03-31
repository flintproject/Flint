/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.executor;

import jp.physiome.Ipc;

public interface ISimulationTrackProcessor {

    public boolean processSimulationTrack(final Ipc.SimulationTrack simTrack);

}
