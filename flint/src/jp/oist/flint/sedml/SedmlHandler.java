/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.sedml;

import jp.physiome.Ipc;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;
import java.math.BigDecimal;
import java.util.ArrayList;

public class SedmlHandler extends DefaultHandler implements ISimulationConfiguration {

    protected boolean mDone;

    protected String mOutputEndTime;
    protected String mNumberOfPoints;
    protected String mGranularity;
    protected String mKisaoId;
    protected String mFilterSyntax;
    protected String mFilterPattern;
    protected String mFilterColumn;

    public SedmlHandler() {
        super();
        mDone = false;
    }

    // implements ISimulationConfiguration

    @Override
    public String getModelCanonicalPath() {
        return ""; // FIXME
    }

    @Override
    public Ipc.IntegrationMethod getIntegrationMethod() {
        if (mKisaoId.equals("KISAO:0000032")) {
            return Ipc.IntegrationMethod.RUNGE_KUTTA;
        } else if (mKisaoId.equals("KISAO:0000280")) {
            return Ipc.IntegrationMethod.ADAMS_MOULTON;
        } else { // KISAO:0000030
            return Ipc.IntegrationMethod.EULER;
        }
    }

    @Override
    public String getLength() {
        return mOutputEndTime;
    }

    @Override
    public String getStep() {
        BigDecimal step = new BigDecimal(mOutputEndTime).divide(new BigDecimal(mNumberOfPoints));
        return step.toString();
    }

    @Override
    public int getGranularity() {
        return Integer.parseInt(mGranularity);
    }

    @Override
    public ArrayList<String> getKeys() {
        ArrayList<String> keys = new ArrayList<>();
        // FIXME
        return keys;
    }

    @Override
    public int getFilterSyntax() {
        return Integer.parseInt(mFilterSyntax);
    }

    @Override
    public String getFilterPattern() {
        return mFilterPattern;
    }

    @Override
    public int getFilterColumn() {
        return Integer.parseInt(mFilterColumn);
    }

    // extends DefaultHandler

    @Override
    public void characters(char[] ch, int start, int end) throws SAXException {
        if (mDone) return;
    }

    @Override
    public void endElement(String uri, String localName, String qName) throws SAXException {
        if (mDone) return;
    }

    @Override
    public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
        if (mDone) return;
        if (localName.equals("filter")) {
            int i = 0;
            while (i < attributes.getLength()) {
                String aName = attributes.getLocalName(i);
                if (aName.equals("syntax")) {
                    mFilterSyntax = attributes.getValue(i);
                } else if (aName.equals("pattern")) {
                    mFilterPattern = attributes.getValue(i);
                } else if (aName.equals("column")) {
                    mFilterColumn = attributes.getValue(i);
                }
                i += 1;
            }
        } else if (localName.equals("uniformTimeCourse")) {
            int i = 0;
            while (i < attributes.getLength()) {
                String aName = attributes.getLocalName(i);
                if (aName.equals("outputEndTime")) {
                    mOutputEndTime = attributes.getValue(i);
                } else if (aName.equals("numberOfPoints")) {
                    mNumberOfPoints = attributes.getValue(i);
                } else if (aName.equals("granularity")) {
                    mGranularity = attributes.getValue(i);
                }
                i += 1;
            }
        } else if (localName.equals("algorithm")) {
            int i = 0;
            while (i < attributes.getLength()) {
                String aName = attributes.getLocalName(i);
                if (aName.equals("kisaoID")) {
                    mKisaoId = attributes.getValue(i);
                }
                i += 1;
            }
            mDone = true;
        }
    }
}
