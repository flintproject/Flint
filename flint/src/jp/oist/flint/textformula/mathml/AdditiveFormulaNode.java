/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.textformula.mathml;

import java.io.IOException;

public class AdditiveFormulaNode extends FormulaNode {

    public AdditiveFormulaNode(String name) {
        super(name);
    }

    @Override
    protected String formulaToString() throws IOException {
        String start = "";
        String end = "";

        StringBuilder sb;
        if (children.size() == 1 && "-".equals(operator)) {
            FormulaNode child = children.get(0);
            start = "("; end = ")";
            if (child instanceof ValueOrVariableFormulaNode
                || child instanceof FunctionFormulaNode) {
                 start = ""; end = "";
            }
            sb = new StringBuilder(operator);
            sb.append(start)
                .append(children.get(0).toFormulaString())
                .append(end);
            return sb.toString();
        }

        if (parent instanceof LogicalFormulaNode
            || parent instanceof RelationalFormulaNode
            || parent instanceof EqualityExpressionFormulaNode
            || parent instanceof MultiveFormulaNode) {
            start = "("; end = ")";
        }

        sb = new StringBuilder(start);
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
        if ("plus".equals(name)) return "+";
        if ("minus".equals(name)) return "-";
        return super.getOperator(name);
    }
}
