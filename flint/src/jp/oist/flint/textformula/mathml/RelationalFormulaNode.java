/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.textformula.mathml;

import java.io.IOException;

public class RelationalFormulaNode extends FormulaNode {

    public RelationalFormulaNode(String name) {
        super(name);
    }

    @Override
    protected String formulaToString() throws IOException {
        String start = "";
        String end = "";

        if (parent instanceof EqualityExpressionFormulaNode
            || parent instanceof RelationalFormulaNode)
            throw new IOException(
                String.format("parse error: invalid use of %s, tag [%s]", operator, name));

        if (parent instanceof LogicalFormulaNode
            || parent instanceof RelationalFormulaNode) {
            start = "("; end = ")";
        }

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
        if ("lt".equals(name))  return "<";
        if ("gt".equals(name))  return ">";
        if ("leq".equals(name))  return "<=";
        if ("geq".equals(name))  return ">=";
        return "";
    }
}
