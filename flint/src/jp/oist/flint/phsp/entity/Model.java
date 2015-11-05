/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.phsp.entity;

import jp.oist.flint.phsp.PhspException;
import jp.physiome.Ipc.ModelLanguage;
import java.io.File;
import java.io.IOException;
import javax.xml.parsers.ParserConfigurationException;

public class Model {
    public static enum ModelFormat {
        PHML, SBML;

        public static ModelFormat valueOf(ModelLanguage lang) throws PhspException {
            if (ModelLanguage.ISML.equals(lang)) 
                return PHML; 

            if (ModelLanguage.SBML.equals(lang)) 
                return SBML;

            throw new PhspException("no corresponding format of language: " + lang.toString());
        }
    }

    private final ModelFormat mFormat;

    private final File mModelFile;

    private final String mOriginalModelPath;

    private String mDescription;

    private ParameterSet mParameterSet = null;

    private TargetSet mTargetSet = null;

    public Model (ModelFormat format, File modelFile) {
        this(format, modelFile, modelFile.getPath(), null, null, null);
    }

    public Model (ModelFormat format, File modelFile, String originalModelPath) {
        this(format, modelFile, originalModelPath, null, null, null);
    }

    public Model (ModelFormat format, File modelFile, String originalModelPath, String description, 
                                ParameterSet parameterSet, TargetSet targetSet) {
        mFormat = format;
        mModelFile = modelFile;
        mOriginalModelPath = originalModelPath;
        mDescription = description;
        mParameterSet = parameterSet;
        mTargetSet  = targetSet;
    }

    public void validate() throws IOException, ParserConfigurationException {
        ParameterSet ps = getParameterSet();
        TargetSet ts = getTargetSet();

        String[] usingParameterNames = ts.getUsingParameterNames();

        boolean hasError = false;

        StringBuilder errors = new StringBuilder();

        for (String name : usingParameterNames) {
            boolean exists = false;
            for (Parameter p : ps.getParameters()) {
                if (p.getName().equals(name)) {
                    exists = true;
                    break;
                }
            }
            if (!exists) {
                hasError = true;
                errors.append(String.format("use of undefined parameter '%s'", name));
                errors.append(System.getProperty("line.separator"));
            }
        }

        if (hasError)
            throw new IOException (errors.toString());

    }

    public ModelFormat getModelFormat () {
        return mFormat;
    }

    public File getModelFile () {
        return mModelFile;
    }

    public String getOriginalModelPath () {
        return mOriginalModelPath;
    }

    public void setDescription (String description) {
        mDescription = description;
    }

    public String getDescription () {
        return mDescription;
    }

    public ParameterSet getParameterSet () {
        return mParameterSet;
    }

    public void setParameterSet (ParameterSet parameterSet) {
        mParameterSet = parameterSet;
    }

    public TargetSet getTargetSet () {
        return mTargetSet;
    }

    public void setTargetSet (TargetSet targetSet) {
        mTargetSet = targetSet;
    }
}
