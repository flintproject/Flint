/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.textformula.mathml;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public abstract class FormulaNode {

    public String operator;

    public final List<FormulaNode> children = new ArrayList<>();

    public final String name;

    public String value = "";

    public FormulaNode parent = null;

    public FormulaNode(String name) {
        this.name = name;
        this.operator = getOperator(name);
    }

    public void addChildNode(FormulaNode node) {
        node.setParentNode(this);
        children.add(node);
    }

    public List<FormulaNode> getChildren() {
        return this.children;
    }

    public boolean hasChildren() {
        return this.children.size() > 0;
    }

    public FormulaNode getParentNode() {
        return this.parent;
    }

    public void setParentNode(FormulaNode parent) {
        this.parent = parent;
    }

    protected String getOperator(String name) {
        return "";
    }

    protected abstract String formulaToString() throws IOException;

    public String toFormulaString() throws IOException {
        return formulaToString();
    }
}
