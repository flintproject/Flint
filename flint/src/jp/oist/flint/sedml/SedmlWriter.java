/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.sedml;

import jp.oist.flint.util.IntegrationMethodFormat;
import java.math.BigDecimal;
import java.io.OutputStreamWriter;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;


public class SedmlWriter {

    private final boolean mFull;

    public SedmlWriter(boolean full) {
        mFull = full;
    }

    private String escape(String s) {
        return s.replaceAll(">","&gt;").replaceAll("<", "&lt;").replaceAll("&", "&amp;").replaceAll("'", "&apos;");
    }

    public void writeSimulationConfiguration(final ISimulationConfigurationList configs, final OutputStream os) 
        throws IOException, SedmlException {

        try (OutputStreamWriter osw = new OutputStreamWriter(os, StandardCharsets.UTF_8);
             BufferedWriter writer = new BufferedWriter(osw)) {
        writer.append("<?xml version='1.0' encoding='UTF-8'?>\n");
        writer.append("<sedML xmlns='http://sed-ml.org/' xmlns:math='http://www.w3.org/1998/Math/MathML' xmlns:flint='http://physiodesigner.org/namespace/flint' version='1' level='1'>\n");

        if (mFull) {
            // FIXME
        }

        int modelCount = configs.getConfigurationCount();

        writer.append("  <listOfSimulations>\n");
        for (int i=0; i<modelCount; i++) {
            ISimulationConfiguration config = configs.getConfiguration(i);
            BigDecimal nop = new BigDecimal(config.getLength()).divide(new BigDecimal(config.getStep()));
            int numberOfPoints = nop.intValueExact();

            writer.append(String.format("    <uniformTimeCourse id='sim%s' name='Simulation %s' initialTime='0' outputStartTime='0' outputEndTime='%s' numberOfPoints='%s' flint:granularity='%s'>\n", 
                    i, i, config.getLength(), numberOfPoints, config.getGranularity()));
            writer.append("      <algorithm kisaoID='KISAO:");
            String kisaoId = IntegrationMethodFormat.kisaoId(config.getIntegrationMethod());
            if (kisaoId == null)
                throw new SedmlException("invalid integration method: " + config.getIntegrationMethod());
            writer.append(kisaoId);
            writer.append("'/>\n");
            writer.append("    </uniformTimeCourse>\n");
        }
        writer.append("  </listOfSimulations>\n");

        writer.append("  <listOfModels>\n");
        for (int i=0; i<modelCount; i++) {
            ISimulationConfiguration config = configs.getConfiguration(i);
            writer.append(String.format("    <model id='model%s' name='Model %s' language='urn:sedml:language:phml' source='", i, i));
            if (mFull) {
                writer.append(escape(config.getModelCanonicalPath()));
            } else {
                writer.append("%%MODEL_PATH%%");
            }
            writer.append("'/>\n");
        }
        writer.append("  </listOfModels>\n");

        writer.append("  <listOfTasks>\n");
        for (int i=0; i<modelCount; i++) {
            ISimulationConfiguration config = configs.getConfiguration(i);
            writer.append(String.format("    <task id='task%s' name='Task %s' modelReference='model%s' simulationReference='sim%s'/>\n", i, i, i, i));
        }
        writer.append("  </listOfTasks>\n");

        writer.append("  <listOfDataGenerators>\n");
        int index = 0;
        for (int i=0; i<modelCount; i++) {
            ISimulationConfiguration config = configs.getConfiguration(i);
            for (String key : config.getKeys()) {
                String kc = key.replace(' ', ':');
                writer.append("    <dataGenerator id='dg" + index + "' name='" + kc + "'>\n");
                writer.append("      <listOfVariables>\n");
                writer.append("        <variable id='v" + index + "' taskReference='task"+i+"' target='" + kc + "'/>\n");
                writer.append("      </listOfVariables>\n");
                writer.append("      <math:math>\n");
                writer.append("        <math:ci>v" + index + "</math:ci>\n");
                writer.append("      </math:math>\n");
                writer.append("    </dataGenerator>\n");
                index += 1;
            }
        }
        writer.append("  </listOfDataGenerators>\n");
        writer.append("</sedML>\n");
        }
    }

    public void writeSimulationConfiguration(final ISimulationConfiguration config, final OutputStream os)
        throws ArithmeticException, IOException, SedmlException {
        BigDecimal nop = new BigDecimal(config.getLength()).divide(new BigDecimal(config.getStep()));
        int numberOfPoints = nop.intValueExact();

        try (OutputStreamWriter osw = new OutputStreamWriter(os, StandardCharsets.UTF_8);
             BufferedWriter writer = new BufferedWriter(osw)) {
        writer.append("<?xml version='1.0' encoding='UTF-8'?>\n");
        writer.append("<sedML xmlns='http://sed-ml.org/' xmlns:math='http://www.w3.org/1998/Math/MathML' xmlns:flint='http://physiodesigner.org/namespace/flint' version='1' level='1'>\n");
        if (mFull) {
            writer.append("  <annotation>\n");
            writer.append("    <flint:filter syntax='" + config.getFilterSyntax() + "' pattern='" + escape(config.getFilterPattern()) + "' column='" + config.getFilterColumn() + "' />\n");
            writer.append("  </annotation>\n");
        }
        writer.append("  <listOfSimulations>\n");
        writer.append("    <uniformTimeCourse id='sim0' name='Simulation 0' initialTime='0' outputStartTime='0' outputEndTime='" + config.getLength() + "' numberOfPoints='" + numberOfPoints + "' flint:granularity='" + config.getGranularity() + "'>\n");
        writer.append("      <algorithm kisaoID='KISAO:");
        String kisaoId = IntegrationMethodFormat.kisaoId(config.getIntegrationMethod());
        if (kisaoId == null)
            throw new SedmlException("invalid integration method: " + config.getIntegrationMethod());
        writer.append(kisaoId);
        writer.append("'/>\n");
        writer.append("    </uniformTimeCourse>\n");
        writer.append("  </listOfSimulations>\n");
        writer.append("  <listOfModels>\n");
        writer.append("    <model id='model0' name='Model 0' language='urn:sedml:language:phml' source='");
        if (mFull) {
            writer.append(escape(config.getModelCanonicalPath()));
        } else {
            writer.append("%%MODEL_PATH%%");
        }
        writer.append("'/>\n");
        writer.append("  </listOfModels>\n");
        writer.append("  <listOfTasks>\n");
        writer.append("    <task id='task0' name='Task 0' modelReference='model0' simulationReference='sim0'/>\n");
        writer.append("  </listOfTasks>\n");
        writer.append("  <listOfDataGenerators>\n");
        int i = 0;
        for (String key : config.getKeys()) {
            String kc = key.replace(' ', ':');
            writer.append("    <dataGenerator id='dg" + i + "' name='" + kc + "'>\n");
            writer.append("      <listOfVariables>\n");
            writer.append("        <variable id='v" + i + "' taskReference='task0' target='" + kc + "'/>\n");
            writer.append("      </listOfVariables>\n");
            writer.append("      <math:math>\n");
            writer.append("        <math:ci>v" + i + "</math:ci>\n");
            writer.append("      </math:math>\n");
            writer.append("    </dataGenerator>\n");
            i += 1;
        }
        writer.append("  </listOfDataGenerators>\n");
        writer.append("</sedML>\n");
        }
    }
}
