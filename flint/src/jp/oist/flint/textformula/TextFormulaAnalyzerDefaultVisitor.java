/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.textformula;

import jp.oist.flint.textformula.analyzer.*;

public class TextFormulaAnalyzerDefaultVisitor
                implements TextFormulaAnalyzerVisitor {

    protected Object defaultVisit(SimpleNode node, Object data) throws ParseException  {
        node.childrenAccept(this, data);
        return data;
    }

    @Override
    public Object visit(SimpleNode node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
    @Override
    public Object visit(AST_Analyze node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
    @Override
    public Object visit(ASTAssignmentNode node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
    @Override
    public Object visit(ASTExpression node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
    @Override
    public Object visit(ASTAddNode node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
    @Override
    public Object visit(ASTSubsNode node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
    @Override
    public Object visit(ASTMultNode node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
    @Override
    public Object visit(ASTDivNode node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
//    @Override
//    public Object visit(ASTModNode node, Object data) throws ParseException {
//        defaultVisit(node, data);
//        return data;
//    }
    @Override
    public Object visit(ASTOrNode node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
    @Override
    public Object visit(ASTAndNode node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
    @Override
    public Object visit(ASTEQNode node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
    @Override
    public Object visit(ASTNENode node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
    @Override
    public Object visit(ASTLTNode node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
    @Override
    public Object visit(ASTGTNode node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
    @Override
    public Object visit(ASTLENode node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
    @Override
    public Object visit(ASTGENode node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
//    @Override
//    public Object visit(ASTArgumentList node, Object data) throws ParseException {
//        defaultVisit(node, data);
//        return data;
//    }
    @Override
    public Object visit(ASTNumber node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
//    @Override
//    public Object visit(ASTDecimalNumber node, Object data) throws ParseException {
//        defaultVisit(node, data);
//        return data;
//    }
//    @Override
//    public Object visit(ASTFloatingNumber node, Object data) throws ParseException {
//        defaultVisit(node, data);
//        return data;
//    }
    @Override
    public Object visit(ASTFunction node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
    @Override
    public Object visit(ASTIdentifier node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
    @Override
    public Object visit(ASTVector node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
    @Override
    public Object visit(ASTVectorList node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
    @Override
    public Object visit(ASTMatrix node, Object data) throws ParseException {
        defaultVisit(node, data);
        return data;
    }
}
