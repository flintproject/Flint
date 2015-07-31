/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.sedml;

import jp.physiome.Ipc;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class SedmlWriterTest {

    private SedmlWriter mSedmlWriter;

    class SimulationConfiguration implements ISimulationConfiguration {

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
            return "10";
        }

        @Override
        public String getStep() {
            return "0.005";
        }

        @Override
        public int getGranularity() {
            return 50;
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

    @BeforeClass
    public static void setUpClass() throws Exception {
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
    }

    @Before
    public void setUp() {
        mSedmlWriter = new SedmlWriter(true);
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testWriteSimulationConfiguration() throws ArithmeticException, IOException {
        SimulationConfiguration config = new SimulationConfiguration();
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        mSedmlWriter.writeSimulationConfiguration(config, baos);
        assertEquals("<?xml version='1.0' encoding='UTF-8'?>\n" +
                     "<sedML xmlns='http://sed-ml.org/' xmlns:math='http://www.w3.org/1998/Math/MathML' xmlns:flint='http://physiodesigner.org/namespace/flint' version='1' level='1'>\n" +
                     "  <annotation>\n" +
                     "    <flint:filter syntax='2' pattern='e2' column='1' />\n" +
                     "  </annotation>\n" +
                     "  <listOfSimulations>\n" +
                     "    <uniformTimeCourse id='sim0' name='Simulation 0' initialTime='0' outputStartTime='0' outputEndTime='10' numberOfPoints='2000' flint:granularity='50'>\n" +
                     "      <algorithm kisaoID='KISAO:0000032'/>\n" +
                     "    </uniformTimeCourse>\n" +
                     "  </listOfSimulations>\n" +
                     "  <listOfModels>\n" +
                     "    <model id='model0' name='Model 0' language='urn:sedml:language:phml' source='/tmp/foo.phml'/>\n" +
                     "  </listOfModels>\n" +
                     "  <listOfTasks>\n" +
                     "    <task id='task0' name='Task 0' modelReference='model0' simulationReference='sim0'/>\n" +
                     "  </listOfTasks>\n" +
                     "  <listOfDataGenerators>\n" +
                     "    <dataGenerator id='dg0' name='028dc220-92ad-11e2-ad45-af668ae8fba2:x'>\n" +
                     "      <listOfVariables>\n" +
                     "        <variable id='v0' taskReference='task0' target='028dc220-92ad-11e2-ad45-af668ae8fba2:x'/>\n" +
                     "      </listOfVariables>\n" +
                     "      <math:math>\n" +
                     "        <math:ci>v0</math:ci>\n" +
                     "      </math:math>\n" +
                     "    </dataGenerator>\n" +
                     "    <dataGenerator id='dg1' name='028dc220-92ad-11e2-ad45-af668ae8fba2:y'>\n" +
                     "      <listOfVariables>\n" +
                     "        <variable id='v1' taskReference='task0' target='028dc220-92ad-11e2-ad45-af668ae8fba2:y'/>\n" +
                     "      </listOfVariables>\n" +
                     "      <math:math>\n" +
                     "        <math:ci>v1</math:ci>\n" +
                     "      </math:math>\n" +
                     "    </dataGenerator>\n" +
                     "    <dataGenerator id='dg2' name='030a3af8-92ad-11e2-b314-b792d07c842c:z'>\n" +
                     "      <listOfVariables>\n" +
                     "        <variable id='v2' taskReference='task0' target='030a3af8-92ad-11e2-b314-b792d07c842c:z'/>\n" +
                     "      </listOfVariables>\n" +
                     "      <math:math>\n" +
                     "        <math:ci>v2</math:ci>\n" +
                     "      </math:math>\n" +
                     "    </dataGenerator>\n" +
                     "  </listOfDataGenerators>\n" +
                     "</sedML>\n", new String(baos.toByteArray(), StandardCharsets.UTF_8));
    }
}
