/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.parameter;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.ExecutionException;


public class SbmlParameterModel extends ParameterModel {

    public final static int INDEX_SPECIES_ID = 0;

    public final static int INDEX_EXPRESSION = 1;

    SbmlParameterModel (File paramFile, File dataFile) 
            throws IOException, InterruptedException, ExecutionException {
        super(paramFile, dataFile);
mColumns = new Object[] {"Species Id", "Expression"};
    }

    @Override
    public boolean isCellEditable (int row, int column) {
        return column == 1;
    }

    @Override
    public Object getValueAt (int row, int column) {
        Parameter p = mParameterList.get(row);
        switch (column) {
        case INDEX_SPECIES_ID: return p.getName();
        case INDEX_EXPRESSION: return p.getValue();
        }
        return null;
    }

    @Override
    public void setValueAt (Object value, int row, int column) {
        Parameter p = mParameterList.get(row);
        String v = (value == null)? "" : value.toString();
        if (column == 1) p.setValue(v);
    }

    public String getSpeciesIdAt (int row) {
        return (String)getValueAt(row, INDEX_SPECIES_ID);
    }

    public String getExpressionAt (int row) {
        return (String)getValueAt(row, INDEX_EXPRESSION);
    }

    public void setExpressionAt (int row, Object expression) {
        setValueAt(expression, row, INDEX_EXPRESSION);
    }
}
