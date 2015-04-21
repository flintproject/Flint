/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.textformula.mathml;

import java.io.IOException;

public class MultiveFormulaNode extends FormulaNode {

    public MultiveFormulaNode(String name) {
        super(name);
        operator = getOperator(name);
    }

    @Override
    protected String formulaToString() throws IOException {
        String start = "";
        String end = "";

        StringBuilder sb = new StringBuilder(start);
        for (int i=0; i<children.size(); i++) {
            FormulaNode node = children.get(i);
            sb.append(node.toFormulaString());
            if (i != children.size()-1)
                sb.append(" "+operator+" ");
        }
        sb.append(end);
        return sb.toString();
    }

    @Override
    protected String getOperator(String name) {
        if ("times".equals(name)) return "*";
        if ("divide".equals(name)) return "/";
        return super.getOperator(name);
    }
}

