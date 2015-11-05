/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.phsp;

import java.io.BufferedWriter;
import java.io.File;
import jp.oist.flint.phsp.entity.Model;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Locale;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;
import jp.oist.flint.phsp.entity.ParameterSet;
import jp.oist.flint.phsp.entity.TargetSet;
import jp.oist.flint.textformula.TextFormula2MathML;
import jp.oist.flint.textformula.analyzer.ParseException;

public class PhspWriter {

    private final static int SHIFT_WIDTH = 2;

    private final String mLineFeed = System.getProperty("line.separator");

    private String escape(String s) {
        return s.replaceAll(">","&gt;").replaceAll("<", "&lt;").replaceAll("&", "&amp;").replaceAll("'", "&apos;");
    }

    protected String indent (int count) {
        StringBuilder sb = new StringBuilder();
        for (int i=0; i<count; i++)
            sb.append(" ");
        return sb.toString();
    }

    public void write (IPhspConfiguration conf, OutputStream os, boolean removeParameterIfNotUsed) 
        throws IOException,
               ParseException,
               ParserConfigurationException,
               PhspException,
               TransformerException {
        try (OutputStreamWriter osw = new OutputStreamWriter(os, StandardCharsets.UTF_8);
             BufferedWriter writer = new BufferedWriter(osw)) {
            StringBuilder sb = new StringBuilder();

            int sw = 0;

            writeLine("<?xml version='1.0' encoding='UTF-8'?>", sw, writer);
            sb.append("<phsp")
                    .append(String.format(" xmlns='%s'", "http://www.physiodesigner.org/2013/ns/phsp/1.0"))
                    .append(String.format(" xmlns:m='%s'", "http://www.w3.org/1998/Math/MathML"))
                    .append(" version='1.0'")
                    .append(">");
            writeLine(sb.toString(), sw, writer);
            sb.setLength(0);
            sw += SHIFT_WIDTH;

            for (Model model : conf.getModels()) {
                String format = model.getModelFormat().name().toLowerCase(Locale.ENGLISH);
                File modelFile = model.getModelFile();
                String path = escape(modelFile.getCanonicalPath());

                writeLine(String.format("<model format='%s' iref='%s'>", format, path), sw, writer);
                sw += SHIFT_WIDTH;

                /* wirter target-set */
                TargetSet ts = model.getTargetSet();
                writeLine("<target-set>", sw, writer);
                sw += SHIFT_WIDTH;

                TargetSet.Target[] targets = ts.getTargets();
                for (TargetSet.Target t : targets) {
                    switch (model.getModelFormat()) {
                    case PHML:
                        sb.append("<target")
                            .append(String.format(" module-id='%s'", t.getModuleId()))
                            .append(String.format(" physical-quantity-id='%s'", t.getPhysicalQuantityId()))
                            .append(">");
                        break;
                    case SBML:
                        sb.append("<target")
                        .append(String.format(" species-id='%s'", t.getSpeciesId()))
                        .append(">");
                        break;
                    default:
                        sw -= SHIFT_WIDTH;
                        continue;
                    }
                    writeLine(sb.toString(), sw, writer);
                    sb.setLength(0);
                    sw += SHIFT_WIDTH;

                    TextFormula2MathML s2mathml = new TextFormula2MathML();
                    s2mathml.parse(new StringReader(t.getValue()));
                    String mathml = s2mathml.getMathML();

                    sb.append("<value>");
                    sb.append(mathml);
                    sb.append("</value>");
                    writeLine(sb.toString(), sw, writer);
                    sb.setLength(0);

                    sw -= SHIFT_WIDTH;
                    writeLine("</target>", sw, writer);
                }
                sw -= SHIFT_WIDTH;
                writeLine("</target-set>", sw, writer);

                /* write parameter-set*/
                ParameterSet ps = model.getParameterSet(); 
                if (removeParameterIfNotUsed)
                    ps = ps.filterByNames(Arrays.asList(ts.getUsingParameterNames()));

                writeLine("<parameter-set>", sw, writer);
                sw += SHIFT_WIDTH;

                ParameterSet.Parameter[] parameters = ps.getParameters();
                if (parameters.length == 0)
                    parameters = new ParameterSet.Dummy().getParameters();

                for (ParameterSet.Parameter p : parameters) {
                    String name = p.getName();
                    writeLine(String.format("<parameter name='%s'>", name), sw, writer);
                    sw += SHIFT_WIDTH;

                    String type = p.getType().name().toLowerCase(Locale.ENGLISH);
                    switch (p.getType()) {
                    case ENUM:
                        sb.append(String.format("<range type='%s'>", type))
                            .append(p.getEnumValue())
                            .append(String.format("</range>"));
                        break;
                    case INTERVAL:
                        sb.append("<range")
                            .append(String.format(" type='%s'", type))
                            .append(String.format(" upper='%s'", p.getRangeUpper()))
                            .append(String.format(" lower='%s'", p.getRangeLower()))
                            .append(String.format(" step='%s'",  p.getRangeStep()))
                            .append("/>");
                        break;
                    default:
                        sw -= SHIFT_WIDTH;
                        continue;
                    }
                    writeLine(sb.toString(), sw, writer);
                    sb.setLength(0);

                    sw -= SHIFT_WIDTH;
                    writeLine("</parameter>", sw, writer);
                }
                sw -= SHIFT_WIDTH;
                writeLine("</parameter-set>", sw, writer);

                sw -= SHIFT_WIDTH;
                writeLine("</model>", sw, writer);
            }

            sw -= SHIFT_WIDTH;
            writeLine("</phsp>", sw, writer);
        }
    }

    private void writeLine(String s, int shiftWidth, BufferedWriter writer) throws IOException {
        writer.append(indent(shiftWidth));
        writer.append(s);
        writer.append(mLineFeed);
    }
}
