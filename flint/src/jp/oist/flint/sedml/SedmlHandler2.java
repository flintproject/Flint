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
                = new ArrayList<>();

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

    private int getIndexBy(Matcher matcher) {
        if (matcher == null)
            return -1;

        for (int i=0; i<mConfigurationList.size(); i++) {
            SimulationConfiguration conf = 
                    (SimulationConfiguration)mConfigurationList.get(i);
            if (matcher.match(conf))
                return i;
        }
        return -1;
    }

    @Override
    public void startElement(String uri, String localName, String qName, Attributes attributes) 
            throws SAXException { 
        super.startElement(uri, localName, qName, attributes);
        switch (localName) {
        case "uniformTimeCourse":
            {
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
            }
            break;
        case "algorithm":
            if (mLastSimId != null) {
                Map data = mSimulationInfos.get(mLastSimId);
                data.put("kisaoId", mKisaoId);
            }
            break;
        case "model":
            {
                int i = 0;
                String id = null;
                String source = null;
                while (i < attributes.getLength()) {
                    String aName = attributes.getLocalName(i);
                    assert aName != null;
                    switch (aName) {
                    case "source":
                        source = attributes.getValue(i);
                        break;
                    case "id":
                        id = attributes.getValue(i);
                        break;
                    }
                    i += 1;
                }
                if (id != null) {
                    Map<String, String> data = new HashMap<>();
                    data.put("source", source);
                    mModelInfos.put(id, data);
                }
            }
            break;
        case "variable":
            {
                int i = 0;
                String target = null;
                String taskId = null;
                while (i < attributes.getLength()) {
                    String aName = attributes.getLocalName(i);
                    assert aName != null;
                    switch (aName) {
                    case "taskReference":
                        taskId = attributes.getValue(i);
                        break;
                    case "target":
                        target = attributes.getValue(i);
                        break;
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
            }
            break;
        case "task":
            {
                int i = 0;
                String modelId = null;
                String simId = null;
                String taskId = null;
                while (i < attributes.getLength()) {
                    String aName = attributes.getLocalName(i);
                    assert aName != null;
                    switch (aName) {
                    case "id":
                        taskId = attributes.getValue(i);
                        break;
                    case "modelReference":
                        modelId = attributes.getValue(i);
                        break;
                    case "simulationReference":
                        simId = attributes.getValue(i);
                        break;
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
            break;
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
        return mConfigurationList.get(getIndexBy(new Matcher<String>(modelPath) {
            @Override
            public boolean match(SimulationConfiguration other) {
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

    private static abstract class Matcher<T> {
        final T mTarget;

        public Matcher(T target) {
            mTarget = target;
        }

        public T getTarget() {
            return mTarget;
        }

        public abstract boolean match(SimulationConfiguration other);
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
            switch (mKisaoId) {
            case "KISAO:0000032":
                return Ipc.IntegrationMethod.RUNGE_KUTTA;
            case "KISAO:0000280":
                return Ipc.IntegrationMethod.ADAMS_MOULTON;
            default:
                // KISAO:0000030
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
