/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.dao;

import java.util.ArrayList;

public class Condition {

    private final String mLogic;

    private final String mSentence;

    private final ArrayList<Condition> mConditions;

    public Condition() {
        this("", "");
    }

    public Condition(String sentence) {
        this("", sentence);
    }

    public Condition(String logic, String sentence) {
        mLogic = logic;
        mSentence = sentence;
        mConditions = new ArrayList<>();
    }

    public void addCondition(Condition condition) {
        mConditions.add(condition);
    }

    public void removeCondition(Condition condition) {
        mConditions.remove(condition);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();

        if (mSentence != null && !mSentence.isEmpty())
            sb.append(mSentence).append(" ");

        if (mLogic != null && !mLogic.isEmpty())
            sb.append(mLogic).append(" ");

        StringBuilder condition = new StringBuilder();
        for (Condition cond : mConditions)
            condition.append(cond.toString());

        String retval;
        if (condition.length() > 0) {
            int lastIndex = mConditions.size() - 1;
            Condition last = mConditions.get(lastIndex);
            String tmp = condition.substring(0, 
                                             condition.length() - (last.mLogic.length()+2));

            sb.append("(").append(tmp).append(")");

            retval = "(" + sb.toString() + ")";

        } else {
            retval = sb.toString();
        }

        return retval;
    }
}
