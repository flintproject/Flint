/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.textformula.mathml;

import jp.oist.flint.textformula.ReservedFunctionContent;
import jp.oist.flint.textformula.ReservedFunctionContent.FunctionInformation;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;
import java.io.Reader;
import java.io.InputStream;
import java.util.List;
import java.util.ArrayList;

public class MathML2TextFormula {

    public final static String MATHML_NAMESPACE_URI = "http://www.w3.org/1998/Math/MathML";

    private final DocumentBuilderFactory mFactory;

    private final MathMLHandler mHandler;

    public MathML2TextFormula() {
        mFactory = DocumentBuilderFactory.newInstance();
        mFactory.setNamespaceAware(true);
        mFactory.setXIncludeAware(false);

        mHandler = new MathMLHandler();
    }

    public void parse(Reader reader)
        throws IOException, ParserConfigurationException, SAXException {
        parse(new InputSource(reader));
    }

    public void parse(InputStream stream)
        throws IOException, ParserConfigurationException, SAXException {
        parse(new InputSource(stream));
    }

    public void parse(InputSource source)
        throws IOException, ParserConfigurationException, SAXException {
            DocumentBuilder builder = mFactory.newDocumentBuilder();
            Document document = builder.parse(source);
            mHandler.parse(document);
    }

    public String getTextFormula () throws IOException {
        return mHandler.getFormulaString();
    }

    private abstract class FormulaNode {

        public String operator;

        public final List<FormulaNode> children = new ArrayList<>();

        public final String name;

        public String value = "";

        public FormulaNode parent = null;

        public FormulaNode (String name) {
            this.name = name; 
            this.operator = getOperator(name);
        }

        public void addChildNode (FormulaNode node) {
            node.setParentNode(this);
            children.add(node);
        }

        public List<FormulaNode> getChildren () {
            return this.children;
        }

        public boolean hasChildren () {
            return this.children.size() > 0;
        }

        public FormulaNode getParentNode () {
            return this.parent;
        }

        public void setParentNode (FormulaNode parent) {
            this.parent = parent;
        }

        protected String getOperator (String name) {
            return "";
        }

        protected abstract String formulaToString () throws IOException;

        public String toFormulaString() throws IOException {
            return formulaToString();
        }
    }

    private class RootFormulaNode extends FormulaNode {

        public RootFormulaNode (String name) {
            super(name);
        }

        @Override
        protected String formulaToString () throws IOException {
            if (children.size() == 1)
                return children.get(0).toFormulaString();
            throw new IOException("math element does NOT have children.");
        }
    }

    private class AssignmentFormulaNode extends FormulaNode {

        public AssignmentFormulaNode (String name) {
            super(name);
        }

        @Override
        protected String formulaToString () throws IOException {
            StringBuilder sb = new StringBuilder();
            if (children.size() == 2) {
                FormulaNode left = children.get(0);
                FormulaNode right = children.get(1);

                sb.append(left.toFormulaString())
                    .append(" "+operator+" ")
                    .append(right.toFormulaString());
                return sb.toString();
            }

            throw new IOException("Missing arguments is 2 for Assignment \"=\"");
        }

        @Override
        protected String getOperator (String name) {
            return "=";
        }
    }

    private class AdditiveFormulaNode extends FormulaNode {

        public AdditiveFormulaNode (String name) {
            super(name);
        }

        @Override
        protected String formulaToString () throws IOException {
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
        protected String getOperator (String name) {
            if ("plus".equals(name)) return "+";
            if ("minus".equals(name)) return "-";
            return super.getOperator(name);
        }
    }

    private class MultiveFormulaNode extends FormulaNode {

        public MultiveFormulaNode (String name) {
            super(name);
            operator = getOperator(name);
        }

       @Override
        protected String formulaToString () throws IOException {
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
        protected String getOperator (String name) {
            if ("times".equals(name)) return "*";
            if ("divide".equals(name)) return "/";
            return super.getOperator(name);
        }
    }

    private class LogicalFormulaNode extends FormulaNode {
  
        public LogicalFormulaNode (String name) {
            super(name);
        }

        @Override
        protected String formulaToString () throws IOException {
            String start = "";
            String end = "";

            if (parent instanceof LogicalFormulaNode) {
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
        protected String getOperator (String name) {
            if ("and".equals(name)) return "&&";
            if ("or".equals(name))  return "||";
            return "";
        }
    }

    private class EqualityExpressionFormulaNode extends FormulaNode {
  
        public EqualityExpressionFormulaNode (String name) {
            super(name);
        }

       @Override
        protected String formulaToString () throws IOException {
            String start = "";
            String end = "";

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
        protected String getOperator (String name) {
            if ("eq".equals(name))   return "==";
            if ("neq".equals(name))  return "!=";
            return "";
        }
    }

    private class RelationalFormulaNode extends FormulaNode {
  
        public RelationalFormulaNode (String name) {
            super(name);
        }

       @Override
        protected String formulaToString () throws IOException {
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
        protected String getOperator (String name) {
            if ("lt".equals(name))  return "<";
            if ("gt".equals(name))  return ">";
            if ("leq".equals(name))  return "<=";
            if ("geq".equals(name))  return ">=";
            return "";
        }
    }

    private class ValueOrVariableFormulaNode extends FormulaNode {
   
        public ValueOrVariableFormulaNode (String name) {
            super(name);
        }

       @Override
        protected String formulaToString () throws IOException {
            return this.value;
        }
    }

    private class CustomFunctionFormulaNode extends FormulaNode {
        public CustomFunctionFormulaNode (String name) {
            super(name);
        }

        @Override
        protected String formulaToString () throws IOException {
            String functionName = this.name;
            int argc = children.size();

            StringBuilder sb = new StringBuilder(functionName).append("(");
            for (int i=1; i<argc; i++) {
                FormulaNode child = children.get(i);
                sb.append(child.toFormulaString());
                if (i != children.size()-1)
                    sb.append(", ");
            }
            sb.append(")");

            return sb.toString();

        }
    }

    private class FunctionFormulaNode extends FormulaNode {
   
        public FunctionFormulaNode (String name) {
            super(name);
        }

        @Override
        protected String formulaToString () throws IOException {
            int argc = children.size();
            List<FunctionInformation> funcInfos =
                ReservedFunctionContent.getFunctionInformationFromMathmlName(name);

            if (funcInfos == null || funcInfos.size() == 0)
                throw new IOException(String.format("%s is not supported yet.", name));

            FunctionInformation funcInfo = null;
            for (FunctionInformation f : funcInfos) {
                if (f.numberOfArguments == argc) {
                    funcInfo = f; break;
                }                
            }

            if (funcInfo == null) {
                String sNumOfArgs = String.valueOf(funcInfos.get(0).numberOfArguments);
                throw new IOException(
                    String.format("Missing arguments %s for `%s` function.", 
                    sNumOfArgs,
                    name));
            }

            String functionName = funcInfo.functionName;
            int startIndex = 0;
            // if logbase is 10, chnage functionName to `log10`
            if ("log".equals(functionName) 
                    && argc == 2 && children.get(0).toFormulaString().equals("10")) {
                    functionName = "log10";
                    startIndex = 1;
            }

            StringBuilder sb = new StringBuilder(functionName).append("(");

            for (int i=startIndex; i<children.size(); i++) {
                FormulaNode child = children.get(i);
                sb.append(child.toFormulaString());
                if (i != children.size()-1)
                    sb.append(", ");
            }
            sb.append(")");

            return sb.toString();
        }
    }

    private class VectorFormulaNode extends FormulaNode {
     
        public VectorFormulaNode (String name) {
            super(name);
        }

       @Override
        protected String formulaToString () throws IOException {
            StringBuilder sb = new StringBuilder("{");
            for (FormulaNode node : children) {
                sb.append(node.toFormulaString())
                    .append(", ");
            }
            String formula = sb.toString();
            formula = formula.substring(0, formula.length()-2);
            return formula + "}";
        }
    }

    private class MatrixFormulaNode extends FormulaNode {
     
        public MatrixFormulaNode (String name) {
            super(name);
        }

        @Override
        protected String formulaToString () throws IOException {
            StringBuilder sb = new StringBuilder("{");
            for (FormulaNode node : children) {
                sb.append(node.toFormulaString())
                    .append(", ");
            }
            String formula = sb.toString();
            formula = formula.substring(0, formula.length()-2);
            return formula + "}";
        }
    }

    private class MatrixRowFormulaNode extends FormulaNode {
     
        public MatrixRowFormulaNode (String name) {
            super(name);
        }

        @Override
        protected String formulaToString () throws IOException {
            StringBuilder sb = new StringBuilder("{");
            for (FormulaNode node : children) {
                sb.append(node.toFormulaString())
                    .append(", ");
            }
            String formula = sb.toString();
            formula = formula.substring(0, formula.length()-2);
            return formula + "}";
        }
    }

    private class MathMLHandler {

        private Document document;

        private FormulaNode root=null, cur;

        private void parse (Document doc) {
            this.document = doc;
                NodeList nodeList = document.getElementsByTagNameNS(MATHML_NAMESPACE_URI, "math");

                if (nodeList.getLength() != 1) return; // throw exception

                Element math = (Element)nodeList.item(0);
                visit(math);
        }

        boolean skipEq = false;

        private void visit(Element element) {
            String localName = element.getLocalName();
            if ("math".equals(localName)) {
                root = new RootFormulaNode(localName);
                cur = root;
                /* 
                 *  For first time, appear the `eq` tag.
                 *  if structure is `math -> apply -> eq`, eq is assignment ( = ).
                 *  In other cases, eq is equality ( == ).
                 */
                Element child = (Element)element.getFirstChild().getFirstChild();
                if (child != null && "eq".equals(child.getLocalName())) {
                    FormulaNode childNode = new AssignmentFormulaNode("eq");
                    cur.addChildNode(childNode);
                    cur = childNode;
                    skipEq = true;
                }    
            } else if (isEqualityTag(localName)) {
                if (!skipEq) {
                    FormulaNode node = new EqualityExpressionFormulaNode(localName);
                    cur.addChildNode(node);
                    cur = node;
                }
                skipEq = false;
            } else if (isAdditiveTag(localName)) {
                FormulaNode node = new AdditiveFormulaNode(localName);
                cur.addChildNode(node);
                cur = node;
            } else if (isMultiveTag(localName)) {
                FormulaNode node = new MultiveFormulaNode(localName);
                cur.addChildNode(node);
                cur = node;
            } else if (isValueOrVariableTag(localName)) {
                FormulaNode node = new ValueOrVariableFormulaNode(localName);
                node.value = element.getTextContent().trim();
                cur.addChildNode(node);
            } else if (isLogicalTag(localName)) {
                FormulaNode node = new LogicalFormulaNode(localName);
                cur.addChildNode(node);
                cur = node;
            } else if (isRelationalTag(localName)) {
                FormulaNode node = new RelationalFormulaNode(localName);
                cur.addChildNode(node);
                cur = node;
            } else if (isFunctionTag(localName)) {
                FormulaNode node = new FunctionFormulaNode(localName);
                if ("log".equals(localName)) {
                    Element child = searchChild((Element)element.getParentNode(), "logbase");
                    if (child != null) {
                        FormulaNode childNode = new ValueOrVariableFormulaNode("logbase");
                        childNode.value = child.getFirstChild().getTextContent().trim();
                        node.addChildNode(childNode);
                    }
                } else if ("root".equals(localName)) {
                    Element child = searchChild((Element)element.getParentNode(), "degree");
                    if (child != null) {
                        FormulaNode childNode = new ValueOrVariableFormulaNode("degree");
                        childNode.value = child.getFirstChild().getTextContent().trim();
                        node.addChildNode(childNode);
                    }
                } else if ("fn".equals(localName)) {
                    node = new CustomFunctionFormulaNode(
                        element.getTextContent().trim());
                }

                cur.addChildNode(node);
                cur = node;
            } else if (isVectorTag(localName)) {
                FormulaNode node = new VectorFormulaNode(localName);
                cur.addChildNode(node);
                cur = node;
            } else if (isMatrixTag(localName)) {
                FormulaNode node = new MatrixFormulaNode(localName);
                cur.addChildNode(node);
                cur = node;
            } else if (isMatrixRowTag(localName)) {
                FormulaNode node = new MatrixRowFormulaNode(localName);
                cur.addChildNode(node);
                cur = node;
            }

            visitChildren(element, element.getChildNodes());
        }

        private void visitChildren(Element parent, NodeList nodeList) {
            int length = nodeList.getLength();
            for (int i=0; i<length; i++) {
                if (nodeList.item(i).getNodeType() != Node.ELEMENT_NODE) continue;
                Element child = (Element)nodeList.item(i);
                if (isIgnoreTag(child.getLocalName())) 
                    continue;

                visit(child);

                if ("apply".equals(child.getLocalName()) && cur != root)
                    cur = cur.getParentNode();

                if ("matrixrow".equals(child.getLocalName().toLowerCase())
                    || "matrix".equals(child.getLocalName().toLowerCase())) {
                     cur = cur.getParentNode();
                }
            }
        }

        private Element searchChild (Element element, String localName) {
            NodeList children = element.getChildNodes();
            int length = children.getLength();
            for (int i=0; i<length; i++) {
                Node item = children.item(i);
                if (item.getNodeType() != Node.ELEMENT_NODE) continue;
                Element child = (Element)item;
                if (localName.equals(child.getLocalName())) {
                    return child;
                }
            }
            return null;
        }

        public String getFormulaString() throws IOException {
            if (root != null)
                return root.toFormulaString();

            return null;
        }

        public boolean isIgnoreTag (String localName) {
            localName = localName.toLowerCase();
            return "logbase".equals(localName) || "degree".equals(localName);
        }

        public boolean isAdditiveTag (String localName) {
            localName = localName.toLowerCase();
            return "plus".equals(localName) || "minus".equals(localName);
        }

        public boolean isMultiveTag (String localName) {
            localName = localName.toLowerCase();
            return "times".equals(localName) || "divide".equals(localName);
        }

        public boolean isEqualityTag (String localName) {
            localName = localName.toLowerCase();
            return "neq".equals(localName) || "eq".equals(localName);
        }

        public boolean isLogicalTag (String localName) {
            localName = localName.toLowerCase();
            return "and".equals(localName) || "or".equals(localName);
        }

        public boolean isRelationalTag (String localName) {
            localName = localName.toLowerCase();
            return "gt".equals(localName) || "lt".equals(localName)
                   || "geq".equals(localName) || "leq".equals(localName);
        }

        public boolean isValueOrVariableTag (String localName) {
            localName = localName.toLowerCase();
            return "cn".equals(localName) || "ci".equals(localName);
        }

        public boolean isFunctionTag (String localName) {
            localName = localName.toLowerCase();
            return (ReservedFunctionContent.indexOfMathmlName(localName) >= 0 ||
                    "fn".equals(localName));
        }

        public boolean isVectorTag (String localName) {
            localName = localName.toLowerCase();
            return "vector".equals(localName);
        }

        public boolean isMatrixTag (String localName) {
            localName = localName.toLowerCase();
            return "matrix".equals(localName);
        }

        public boolean isMatrixRowTag (String localName) {
            localName = localName.toLowerCase();
            return "matrixrow".equals(localName);
        }
    }
}
