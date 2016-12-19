/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.sedml;

import jp.physiome.Ipc;
import java.util.ArrayList;
import org.junit.Test;
import static org.junit.Assert.*;

public class UniformTimeCourseTest {

    private static class SimulationConfiguration implements ISimulationConfiguration {

        String mLength;
        String mStep;

        public SimulationConfiguration(String length, String step) {
            mLength = length;
            mStep = step;
        }

        @Override
        public String getModelCanonicalPath() {
            return "/tmp/foo.phml";
        }

        @Override
        public Ipc.IntegrationMethod getIntegrationMethod() {
            return Ipc.IntegrationMethod.RUNGE_KUTTA;
        }

        @Override
        public String getLength() {
            return mLength;
        }

        @Override
        public String getStep() {
            return mStep;
        }

        @Override
        public int getGranularity() {
            return 50;
        }

        @Override
        public String getOutputStartTime() {
            return "3.14";
        }

        @Override
        public ArrayList<String> getKeys() {
            ArrayList<String> keys = new ArrayList<>();
            keys.add("028dc220-92ad-11e2-ad45-af668ae8fba2:x");
            keys.add("028dc220-92ad-11e2-ad45-af668ae8fba2:y");
            keys.add("030a3af8-92ad-11e2-b314-b792d07c842c:z");
            return keys;
        }

        @Override
        public int getFilterSyntax() {
            return 2;
        }

        @Override
        public String getFilterPattern() {
            return "e2";
        }

        @Override
        public int getFilterColumn() {
            return 1;
        }
    }

    @Test
    public void testConstructor() {
        SimulationConfiguration config = new SimulationConfiguration("100", "0");
        try {
            UniformTimeCourse utc = new UniformTimeCourse(config);
            fail("SedmlException expected");
        } catch (SedmlException se) {
            // OK
        }
    }

    @Test
    public void testGetNumberOfPoints() throws SedmlException {
        SimulationConfiguration config = new SimulationConfiguration("100", "0.002");
        UniformTimeCourse utc = new UniformTimeCourse(config);
        assertEquals(50000, utc.getNumberOfPoints());
    }
}
