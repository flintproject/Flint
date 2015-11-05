/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.form.job;

import jp.oist.flint.phsp.PhspUtility;
import jp.oist.flint.phsp.entity.Parameter;
import jp.oist.flint.phsp.entity.ParameterSet;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.swing.AbstractListModel;

public class CombinationModel extends AbstractListModel
                                implements IParameterInfo {

    private final ArrayList<Number[]> mCombination;

    private Number[] mMaximumValues = null;

    private Number[] mMinimumValues = null;

    private Number[] mBuffer = null;

    private String[] mTitles = null;

    private boolean mIsDummy = false;

    private final List<Integer> mIgnoreIndexes;

    private int mCombinationSize = 0; 

    public CombinationModel () {
        mCombination = new ArrayList<>();
        mIgnoreIndexes = new ArrayList<>();

        parseParameterSet(null, null);
    }

    private void parseParameterSet (ParameterSet pset, ParameterFilter filter) {
        if (pset == null || pset.size() <= 0) 
            return;

        int columnCount = pset.size();

        mMaximumValues = new Number[columnCount];
        Arrays.fill(mMaximumValues, null);
        mMinimumValues = new Number[columnCount];
        Arrays.fill(mMinimumValues, null);

        Number[][] parameters = new Number[pset.size()][];
        for(int i=0; i<columnCount; i++) {
            Parameter p = pset.get(i);

            switch (p.getType()) {
                case ENUM:
                    String enumVal = p.getEnumValue();
                    parameters[i] = PhspUtility.getEnumValue(enumVal);
                    break;
                case INTERVAL:
                    String step    = p.getRangeStep();
                    String upper   = p.getRangeUpper();
                    String lower   = p.getRangeLower();
                    parameters[i] = PhspUtility.getIntervalValue(lower, upper, step);
                    break;
            }

            if (filter != null && !filter.accept(parameters[i]))
                mIgnoreIndexes.add(i);
        }

        parse(parameters);
    }

    private void parse (Number[][] arrays) {
        int columnCount = arrays.length;
        mMaximumValues = new Number[columnCount];
        Arrays.fill(mMaximumValues, null);
        mMinimumValues = new Number[columnCount];
        Arrays.fill(mMaximumValues, null);

        for (int i=0; i<arrays.length; i++) {
            for (Number val : arrays[i]) {
                if (mMaximumValues[i] == null) {
                    mMaximumValues[i] = val;
                } else {
                    mMaximumValues[i] = Math.max(mMaximumValues[i].doubleValue(), val.doubleValue());
                }

                if (mMinimumValues[i] == null) {
                    mMinimumValues[i] = val;
                } else {
                    mMinimumValues[i] = Math.min(mMinimumValues[i].doubleValue(), val.doubleValue());
                }
            }
        }

        mBuffer = new Number[columnCount];
        parse(arrays, 0);
    }

    private void  parse (Number[][] arrays, int index) {
        if (index >= arrays.length) {
            addElement(mBuffer.clone());
        } else {
            for (Number item : arrays[index]) {
                mBuffer[index] = item;
                parse(arrays, index+1);
            }
        }
    }

    private Number[] constructParameter (Number[] values) {
        List<Number> tmp = new ArrayList<>();
        for (int i=0; i<values.length; i++) {
            if (mIgnoreIndexes.contains(i))
                continue;
            tmp.add(values[i]);
        }

        return tmp.toArray(new Number[tmp.size()]);
    }

    public void setParameterIsDummy (boolean isDummy) {
        mIsDummy = isDummy;
    }

    public boolean getParameterIsDummy () {
        return mIsDummy;
    }

    public void load (ParameterSet pset) {
        load(pset, null);
    }

    public void load (ParameterSet pset, ParameterFilter filter) {

        removeAll();

        mTitles = null;
        mBuffer = null;

        if (pset == null || pset.size() <= 0)
            return;

        mTitles = new String[pset.size()];
        int combinationSize = -1;
        for (int i=0; i<pset.size(); i++) { 
            Parameter p = pset.get(i);
            mTitles[i] = p.getName();
        }

        mCombinationSize = PhspUtility.calculateCombinationCount(pset);

        parseParameterSet(pset, filter);
    }

    @Override
    public int getSize() {
        return mCombinationSize;
    }

    @Override
    public Object getElementAt(int index) {
        if (index >= mCombination.size())
            return null;

        return constructParameter(mCombination.get(index));
    }

    public Number[] getValues (int index) {
        return mCombination.get(index);
    }

    public void addElement (Number[] o) {
        mCombination.add(o);
        int lastIndex = mCombination.size() - 1;
        fireIntervalAdded(this, lastIndex, lastIndex);
    }

    public Number[] removeElement (Number[] o) {
        int index = indexOf(o);

        if (index <= 0)
            return null;

        Number[] oldValue = mCombination.remove(index);
        this.fireIntervalRemoved(this, index, index);

        return oldValue;
    }

    public void removeAll () {
        int last = mCombination.size()-1;
        mCombinationSize = 0;
        mIgnoreIndexes.clear();
        mCombination.clear();
        mMaximumValues = null;
        mMinimumValues = null;

        if (last >= 0) 
            this.fireIntervalRemoved(this, 0, last);
    }

    public int indexOf (Number[] target) {
        int size = mCombination.size();
        for (int i=0; i<size; i++) {
            if (Arrays.equals(mCombination.get(i), target))
                return i;
        }
        return -1;
    }

    /*
     * if count of parameter is 1, that was ignored.
     */
    @Override
    public Integer[] getIgnoreIndexes () {
        int size = mIgnoreIndexes.size();
        return mIgnoreIndexes.toArray(new Integer[size]);
    }

    @Override
    public int[] getActiveIndexes () {
        int [] indexes = new int[getActiveParameterCount()];

        for (int i=0, j=0; i<getParameterCount(); i++) {
            if (mIgnoreIndexes.contains(i))
                continue;

            indexes[j] = i; j++;
        }
        return indexes;
    }

    @Override
    public int getParameterCount () {
        if (mTitles == null)
            return -1;
        return mTitles.length;
    }

    @Override
    public int getActiveParameterCount () {
        if (mTitles == null)
            return -1;
        return mTitles.length - mIgnoreIndexes.size();
    }

    @Override
    public String[] getTitles () {
        return mTitles;
    }

    @Override
    public String[] getActiveTitles () {
        if (mTitles == null) 
            return null;
        String[] titles = new String[getActiveParameterCount()];
        int[] indexes = getActiveIndexes();
        for (int i=0; i<indexes.length; i++)
            titles[i] = mTitles[indexes[i]];

        return titles;
    }

    @Override
    public Number[] getActiveMaximums () {
        if (mMaximumValues == null && getActiveParameterCount() < 0) 
            return null;

        Number[] maximums = new Number[getActiveParameterCount()];
        int[] indexes = getActiveIndexes();
        for (int i=0; i<indexes.length; i++)
            maximums[i] = mMaximumValues[indexes[i]];

        return maximums;
    }

    @Override
    public Number[] getActiveMinimums () {
        if (mMinimumValues == null) 
            return null;

        Number[] minimums = new Number[getActiveParameterCount()];
        int[] indexes = getActiveIndexes();
        for (int i=0; i<indexes.length; i++)
            minimums[i] = mMinimumValues[indexes[i]];

        return minimums;
    }

    @Override
    public String getTitle (int index) {
        if (mTitles == null)
            return null;
        return mTitles[index];
    }

    @Override
    public Number getMaximum (int index) {
        return mMaximumValues[index];
    }

    @Override
    public Number getMinimum (int index) {
        return mMinimumValues[index];
    }
}