/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */

package jp.oist.flint.sedml;

import jp.physiome.Ipc;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SedmlHandler2 extends SedmlHandler 
    implements ISimulationConfigurationList {

    private final List<ISimulationConfiguration> mConfigurationList 
                = new ArrayList<ISimulationConfiguration>();

    private final HashMap<String, Map> mModelInfos;

    private final HashMap<String, Map> mSimulationInfos;

    private final HashMap<String, Map> mTaskInfos;

    private String mLastSimId;
    
    public SedmlHandler2 () {
        super();
        mDone = false;
        mModelInfos = new HashMap<>();
        mSimulationInfos = new HashMap<>();
        mTaskInfos = new HashMap<>();

        mFilterSyntax  = "1";
        mFilterPattern = "*";
        mFilterColumn  = "0";
    }

    private int getIndexBy (Equalator equalator) {
        if (equalator == null)
            return -1;

        for (int i=0; i<mConfigurationList.size(); i++) {
            SimulationConfiguration conf = 
                    (SimulationConfiguration)mConfigurationList.get(i);
            if (equalator.equals(conf))
                return i;
        }
        return -1;
    }

    @Override
    public void startElement(String uri, String localName, String qName, Attributes attributes) 
            throws SAXException { 
        super.startElement(uri, localName, qName, attributes);
        if (localName.equals("uniformTimeCourse")) {
            int i = 0;
            String id = null;
            while (i < attributes.getLength()) {
                String aName = attributes.getLocalName(i);
                if ("id".equals(aName)) {
                    id = attributes.getValue(i); 
                    break;
                }
                i += 1;
            }
            if (id != null) {
                Map<String, String> data = new HashMap<>();
                data.put("outputEndTime", mOutputEndTime);
                data.put("numberOfPoints", mNumberOfPoints);
                data.put("granularity", mGranularity);
                mSimulationInfos.put(id, data);
                mLastSimId = id;
            }

        } else if (localName.equals("algorithm")) {
            if (mLastSimId != null) {
                Map data = mSimulationInfos.get(mLastSimId);
                data.put("kisaoId", mKisaoId);
            }
        } else if (localName.equals("model")) {
            int i = 0;
            String id = null;
            String source = null;
            while (i < attributes.getLength()) {
                String aName = attributes.getLocalName(i);
                if ("source".equals(aName)) {
                    source = attributes.getValue(i);
                } else if ("id".equals(aName)) {
                    id = attributes.getValue(i); 
                }
                i += 1;
            }
            if (id != null) {
                Map<String, String> data = new HashMap<>();
                data.put("source", source);
                mModelInfos.put(id, data);
            }
        } else if (localName.equals("variable")) {
            int i = 0;
            String target = null;
            String taskId = null;
            while (i < attributes.getLength()) {
                String aName = attributes.getLocalName(i);
                if ("taskReference".equals(aName)) {
                    taskId = attributes.getValue(i);
                } else if ("target".equals(aName)) {
                    target = attributes.getValue(i);
                }
                i += 1;
            }

            if (taskId != null && !taskId.isEmpty()) {
                Map data;
                if (!mTaskInfos.containsKey(taskId))
                    mTaskInfos.put(taskId, new HashMap<String, Object>());
                data = mTaskInfos.get(taskId);
                if (!data.containsKey("targets"))
                    data.put("targets", new ArrayList<String>());
                List targets = (List)data.get("targets");
                targets.add(target);
            }

        } else if (localName.equals("task")) {
            int i = 0;
            String modelId = null;
            String simId = null;
            String taskId = null;
            while (i < attributes.getLength()) {
                String aName = attributes.getLocalName(i);
                if ("id".equals(aName)) {
                    taskId = attributes.getValue(i);
                } else if ("modelReference".equals(aName)) {
                    modelId = attributes.getValue(i);
                } else if ("simulationReference".equals(aName)) {
                    simId = attributes.getValue(i);
                }
                i += 1;
            }
            if (taskId != null && !taskId.isEmpty()) { 
                if (!mTaskInfos.containsKey(taskId))
                    mTaskInfos.put(taskId, new HashMap<String, Object>());

                Map data = mTaskInfos.get(taskId);

                data.put("modelReference", modelId);
                data.put("simulationReference", simId);
            }
        }
    }

    @Override
    public void endDocument () {
        for (Map.Entry entry : mTaskInfos.entrySet()) {
            Map<String, Object> data = (Map<String, Object>)entry.getValue();

            String simId   = (String)data.get("simulationReference");
            String modelId = (String)data.get("modelReference");

            Map<String, String> simInfo = mSimulationInfos.get(simId);
            Map<String, String> modelInfo = mModelInfos.get(modelId);

            SimulationConfiguration conf = new SimulationConfiguration();
            conf.mUnitformTimeCourseId = simId;
            conf.mKisaoId        = (String)simInfo.get("kisaoId");
            conf.mOutputEndTime  = (String)simInfo.get("outputEndTime");
            conf.mNumberOfPoints = (String)simInfo.get("numberOfPoints");
            conf.mGranularity    = (String)simInfo.get("granularity");

            conf.mModelId = modelId;
            conf.mModelPath = (String)modelInfo.get("source");

            conf.mKeys = (ArrayList<String>)data.get("targets");

            mConfigurationList.add(conf);
        }
    }

    @Override
    public int getConfigurationCount () {
        return mConfigurationList.size();
    }

    @Override
    public ISimulationConfiguration getConfiguration(int index) {
        return mConfigurationList.get(index);
    }

    @Override
    public ISimulationConfiguration getConfigurationByModelPath(String modelPath) {
        return mConfigurationList.get(getIndexBy(new Equalator<String>(modelPath) {
            @Override
            public boolean equal(SimulationConfiguration other) {
                return getTarget().equals(other.getModelCanonicalPath());
            }
        }));
    }

    @Override
    public String getModelCanonicalPath() {
        int lastIndex = mConfigurationList.size() -1;
        if (lastIndex < 0)
            return null;
        return mConfigurationList.get(lastIndex).getModelCanonicalPath();
    }

    @Override
    public List<ISimulationConfiguration> toList() {
        return Collections.unmodifiableList(mConfigurationList);
    }

    private static abstract class Equalator <T> {
        final T mTarget;

        public Equalator (T target) {
            mTarget = target;
        }

        public T getTarget () {
            return mTarget;
        }

        public abstract boolean equal (SimulationConfiguration other);
    }

    private static class SimulationConfiguration 
        implements ISimulationConfiguration {

        private String mModelId;
        private String mUnitformTimeCourseId;

        private String mModelPath;
        private String mOutputEndTime;
        private String mNumberOfPoints;
        private String mGranularity;
        private String mKisaoId;
        private String mFilterSyntax  = "1";
        private String mFilterPattern = "*";
        private String mFilterColumn  = "0";

        private ArrayList<String> mKeys = new ArrayList<>();

        public SimulationConfiguration () { }

        @Override
        public String getModelCanonicalPath() {
            return mModelPath;
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

        void setIntegrationMethod (String kisaoId) {
            mKisaoId = kisaoId;
        }

        @Override
        public String getLength() {
            return mOutputEndTime;
        }

        void setLength (String outputEndTime) {
            mOutputEndTime = outputEndTime;
        }

        @Override
        public String getStep() {
            BigDecimal step = new BigDecimal(mOutputEndTime).divide(new BigDecimal(mNumberOfPoints));
            return step.toString();
        }

        void setNumberOfPoints (String numberOfPoints) {
            mNumberOfPoints = numberOfPoints;
        }

        @Override
        public int getGranularity() {
            return Integer.parseInt(mGranularity);
        }

        void setGranularity (String granularity) {
            mGranularity = granularity;
        }

        @Override
        public ArrayList<String> getKeys() {
            return mKeys;
        }

        @Override
        public int getFilterSyntax() {
            try {
                return Integer.parseInt(mFilterSyntax);
            } catch (NumberFormatException ex) {
                return -1;
            }
        }

        void setFilterSyntax (String filterSyntax) {
            mFilterSyntax = filterSyntax;
        }

        @Override
        public String getFilterPattern() {
            return mFilterPattern;
        }

        void setFilterPattern (String filterPattern) {
            mFilterPattern = filterPattern;
        }

        @Override
        public int getFilterColumn() {
            try {
                return Integer.parseInt(mFilterColumn);
            } catch (NumberFormatException ex) {
                return -1;
            }
        }

        void setFilterColumn (String filterColumn) {
            mFilterColumn = filterColumn;
        }
    }
}
