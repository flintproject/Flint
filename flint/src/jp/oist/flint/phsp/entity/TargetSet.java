/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.phsp.entity;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;
import jp.oist.flint.textformula.TextFormula2MathML;
import jp.oist.flint.textformula.analyzer.ParseException;

public class TargetSet {

    private ArrayList<Target> mTargets = new ArrayList<>();

    public Target[] getTargets () {
        return (Target[]) mTargets.toArray(new Target[mTargets.size()]);
    }

    public void add (TargetSet.Target target) {
        mTargets.add(target);
    }

    public int size() {
        return mTargets.size();
    }

    public String[] getUsingParameterNames() throws IOException, ParserConfigurationException {
        LinkedHashSet<String> usingParameterNames = new LinkedHashSet<>();
        TextFormula2MathML s2mathml = new TextFormula2MathML();
        for (Target t : mTargets) {
            try {
                s2mathml.parse(new StringReader(t.getValue()));
                usingParameterNames.addAll(s2mathml.getUsingParameterNames());
            } catch (ParseException ex) {
                throw new IOException("Invalid parameter in expression", ex);
            } catch (TransformerException ex) {
                throw new IOException("Could not generate the phsp file ", ex);
            }
        }
        return usingParameterNames.toArray(new String[usingParameterNames.size()]);
    }

    public static class Target {
        private String mModuleId;

        private String mModuleName;

        private String mPhysicalQuantityId;

        private String mPhysicalQuantityName;

        private String mSpeciesId;

        private String mValue;

        public Target () {
            mModuleId = "";
            mModuleName = "";
            mPhysicalQuantityId = "";
            mPhysicalQuantityName = "";
            mSpeciesId = "";
            mValue = "";
        }

        public String getModuleId() {
            return mModuleId;
        }

        public void setModuleId (String moduleId) {
            mModuleId = moduleId;
        }

        public String getModuleName () {
            return mModuleName;
        }

        public void setModuleName (String moduleName) {
            mModuleName = moduleName;
        }

        public String getPhysicalQuantityId() { 
            return mPhysicalQuantityId;
        }

        public void setPhysicalQuantityId (String pqId) {
            mPhysicalQuantityId = pqId;
        }

        public String getPhysicalQuantityName() { 
            return mPhysicalQuantityName;
        }

        public void setPhysicalQuantityName (String pqName) {
            mPhysicalQuantityName = pqName;
        }

        public String getSpeciesId() {
            return mSpeciesId;
        }

        public void setSpeciesId (String sId) {
            mSpeciesId = sId;
        }

        public String getValue() {
            return mValue;
        }

        public void setValue (String value) {
            mValue = value;
        }
    }
}
