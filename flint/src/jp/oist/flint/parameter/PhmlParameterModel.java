/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */

package jp.oist.flint.parameter;

import jp.physiome.Lo.Type;
import java.io.File;
import java.io.IOException;
import java.util.concurrent.ExecutionException;

public class PhmlParameterModel extends ParameterModel {

    public final static int INDEX_MODULE_NAME = 0;

    public final static int INDEX_PQ_NAME     = 1;

    public final static int INDEX_PQ_TYPE     = 2;

    public final static int INDEX_EXPRESSION  = 3;

    public final static int INDEX_MODULE_ID   = 4;

    public final static int INDEX_PQ_ID       = 5;

    PhmlParameterModel (File paramFile, File dataFile) 
            throws IOException, InterruptedException, ExecutionException {
        super(paramFile, dataFile);

        mColumns = new Object[] { "Module Name",  "PQ Name", "PQ Type", "Expression", "Module ID", "PQ ID" };
    }

    @Override
    public boolean isCellEditable (int row, int column) {
        return column == INDEX_EXPRESSION;
    }

    @Override
    public Object getValueAt (int row, int column) {
        Parameter p = mParameterList.get(row);
        switch (column) {
        case INDEX_MODULE_NAME: return p.getTrackName();
        case INDEX_PQ_NAME:     return p.getName();
        case INDEX_PQ_TYPE:
            return (p.getType() == Type.S) ? "static-parameter" : "initial-value";
        case INDEX_EXPRESSION:  return p.getValue();
        case INDEX_MODULE_ID:   return p.getUuid();
        case INDEX_PQ_ID:       return p.getId();
        }
        return null;
    }

    @Override
    public void setValueAt (Object value, int row, int column) {
        Parameter p = mParameterList.get(row);
        String v = (value == null)? "" : value.toString();
        if (column == INDEX_EXPRESSION) p.setValue(v);
    }

    public String getModuleIdAt (int row) {
        return (String)getValueAt(row, INDEX_MODULE_ID); 
    }

    public String getModuleNameAt (int row) {
        return (String)getValueAt(row, INDEX_MODULE_NAME); 
    }

    public String getPhysicalQuantityIdAt (int row) {
        return (String)getValueAt(row, INDEX_PQ_ID); 
    }

    public String getPhysicalQuantityNameAt (int row) {
        return (String)getValueAt(row, INDEX_PQ_NAME); 
    }

    public String getPhysicalQuantityTypeAt(int row) {
        return (String)getValueAt(row, INDEX_PQ_TYPE);
    }

    public String getExpressionAt (int row) {
        return (String)getValueAt(row, INDEX_EXPRESSION);
    }

    public void setExpressionAt (int row, Object expression) {
        setValueAt(expression, row, INDEX_EXPRESSION);
    }
}
