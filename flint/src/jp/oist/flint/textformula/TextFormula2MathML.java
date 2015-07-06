/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.textformula;

import jp.oist.flint.textformula.analyzer.*;
import jp.oist.flint.textformula.mathml.*;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.OutputKeys;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.DOMImplementation;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import javax.xml.transform.TransformerException;

public class TextFormula2MathML
            extends TextFormulaAnalyzerDefaultVisitor {

    private final TextFormulaAnalyzer mAnalyzer;

    private Document mDocument;

    private boolean mUsingNamespaceURI = false;

    private final ArrayList<String> mUsingParameterNames; 

    public TextFormula2MathML () {
        mAnalyzer = new TextFormulaAnalyzer();
        mAnalyzer.setVisitor(this);

        mUsingParameterNames = new ArrayList();
    }

    public void setUsingNamespaceURI (boolean b) {
            mUsingNamespaceURI = b;
    }

    public boolean isUsingNamespaceURI () {
        return mUsingNamespaceURI;
    }

    public Document parse(java.io.Reader reader)
        throws ParseException, ParserConfigurationException {
        mDocument = newDocument();
        mAnalyzer.analyze(reader);
        return mDocument;
    }        

    public Document parse(java.io.InputStream stream)
        throws ParseException, ParserConfigurationException {
        mDocument = newDocument();
        mAnalyzer.analyze(stream);
        return mDocument;
    }

    public Document parse (java.io.InputStream stream, String encoding) 
        throws ParseException, ParserConfigurationException {
        mDocument = newDocument();
        mAnalyzer.analyze(stream, encoding);
        return mDocument;
    }

    public String getMathML() throws TransformerException {
        return value2mathml();
    }

    public List getUsingParameterNames () 
            throws TransformerException {
        value2mathml();
        return mUsingParameterNames;
    }


    public Document getResult () {
        return mDocument;
    }

    private String value2mathml () 
            throws TransformerException {
        DOMSource domSource = new DOMSource(mDocument);
        StringWriter writer = new StringWriter();
        StreamResult result = new StreamResult(writer);
        TransformerFactory tf = TransformerFactory.newInstance();
        Transformer transformer = tf.newTransformer();
        transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
        transformer.transform(domSource, result);
        return writer.toString();
    }

    private Document newDocument() throws ParserConfigurationException {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setNamespaceAware(true);
            DocumentBuilder builder = factory.newDocumentBuilder();
            DOMImplementation impl = builder.getDOMImplementation();

            String namespaceURI = (mUsingNamespaceURI)? MathML2TextFormula.MATHML_NAMESPACE_URI : "";

            return impl.createDocument(namespaceURI, "m:math", null);
    }

    public void dump () {
        mAnalyzer.dump();
    }

    public void dump (String s) {
        mAnalyzer.dump(s);
    }

    protected Element visitOperator (SimpleNode node, Element parent, String sOperator) {
        Element apply = mDocument.createElement("m:apply");
        Element operator = mDocument.createElement(sOperator);
        apply.appendChild(operator);
        parent.appendChild(apply);
        return apply;
    } 

    /*****************************************************
     * implements TextFormulaAnalyzerDefaultVisitor *
     *****************************************************/
    /*
     * Start parsing
     */
    @Override
    public Object visit(AST_Analyze node, Object data) 
        throws ParseException {
        Element elmt = mDocument.getDocumentElement();
        defaultVisit(node, elmt);
        return data;
    }

    /*
     * assignment
     * Identifier "=" Expression | Identifier | Number ...
     */
    @Override
    public Object visit(ASTAssignmentNode node, Object data) 
        throws ParseException {
        Element apply = visitOperator(node, (Element)data, "m:eq");
        defaultVisit(node, apply);
        return data;
    }

    /*
     *  operator "+" 
     *  ex.) y = x + 4
     */
    @Override
    public Object visit(ASTAddNode node, Object data) 
        throws ParseException {
        Element apply = visitOperator(node, (Element)data, "m:plus");
        defaultVisit(node, apply);
        return data;
    }

    /*
     *  operator "-" 
     *  ex.) y = a - 1
     */
    @Override
    public Object visit(ASTSubsNode node, Object data) 
        throws ParseException {
        Element apply = visitOperator(node, (Element)data, "m:minus");
        defaultVisit(node, apply);
        return data;
    }
    /*
     *  operator "*" 
     *  ex.) y = x * 3
     */
    @Override
    public Object visit(ASTMultNode node, Object data) 
        throws ParseException {
        Element apply = visitOperator(node, (Element)data, "m:times");
        defaultVisit(node, apply);
        return data;
    }

    /*
     *  operator "/" 
     *  ex.) y = 1 / 2
     */
    @Override
    public Object visit(ASTDivNode node, Object data) 
        throws ParseException {
        Element apply = visitOperator(node, (Element)data, "m:divide");
        defaultVisit(node, apply);
        return data;
    }

    /*
     * operator "%"
     *  ex.) y = a % 4
     */
//    @Override
//    public Object visit(ASTModNode node, Object data) 
//        throws ParseException {
//        Element apply = visitOperator(node, (Element)data, "m:rem");
//        defaultVisit(node, apply);
//        return data;
//    }
//
    /*
     * condition operator "||"
     * ex.) y || x
     */
    @Override
    public Object visit(ASTOrNode node, Object data) 
        throws ParseException {
        Element apply = visitOperator(node, (Element)data, "m:or");
        defaultVisit(node, apply);
        return data;
    }

    /*
     * condition operator "&&"
     * ex.) y && x
     */
    @Override
    public Object visit(ASTAndNode node, Object data) 
        throws ParseException {
        Element apply = visitOperator(node, (Element)data, "m:and");
        defaultVisit(node, apply);
        return data;
    }

    /*
     * equality operator "=="
     * ex.) y == x
     */
    @Override
    public Object visit(ASTEQNode node, Object data) 
        throws ParseException {
        Element apply = visitOperator(node, (Element)data, "m:eq");
        defaultVisit(node, apply);
        return data;
    }

    /*
     * equality operator "!="
     * ex.) y != x
     */
    @Override
    public Object visit(ASTNENode node, Object data) 
        throws ParseException {
        Element apply = visitOperator(node, (Element)data, "m:neq");
        defaultVisit(node, apply);
        return data;
    }

    /*
     * relational operator "<"
     * ex.) y < x
     */
    @Override
    public Object visit(ASTLTNode node, Object data) 
        throws ParseException {
        Element apply = visitOperator(node, (Element)data, "m:lt");
        defaultVisit(node, apply);
        return data;
    }

    /*
     * relational operator ">"
     * ex.) y > x
     */
    @Override
    public Object visit(ASTGTNode node, Object data) 
        throws ParseException {
        Element apply = visitOperator(node, (Element)data, "m:gt");
        defaultVisit(node, apply);
        return data;
    }

    /*
     * relational operator "<="
     * ex.) y <= x
     */
    @Override
    public Object visit(ASTLENode node, Object data) 
        throws ParseException {
        Element apply = visitOperator(node, (Element)data, "m:leq");
        defaultVisit(node, apply);
        return data;
    }

    /*
     * relational operator ">="
     * ex.) y >= x
     */
    @Override
    public Object visit(ASTGENode node, Object data) 
        throws ParseException {
        Element apply = visitOperator(node, (Element)data, "m:geq");
        defaultVisit(node, apply);
        return data;
    }

    /*
     * number
     * ex.) 1, 123, 0.1, 1.2 ...
     */
    @Override
    public Object visit(ASTNumber node, Object data) 
        throws ParseException {
        Element parent = (Element)data;
        Element number = mDocument.createElement("m:cn");
        number.setTextContent((String)node.jjtGetValue());
        parent.appendChild(number);
        defaultVisit(node, data);
        return data;
    }

    /*
     * reserved function
     * ex.)  log(2,3) pow(x,2), abs(val1)....
     */
    @Override
    public Object visit(ASTFunction node, Object data) 
        throws ParseException {
        String functionName = (String)node.jjtGetValue();
        int numberOfArguments = node.numberOfArguments;

        java.util.List<ReservedFunctionContent.FunctionInformation> funcInfos = 
            ReservedFunctionContent.getFunctionInformationFromFunctionName(functionName);


        if (funcInfos == null || funcInfos.isEmpty())
            throw new ParseException(
                String.format("`%s` functions are not available.", 
                functionName));
        ReservedFunctionContent.FunctionInformation funcInfo = null;

        String sNumOfArgs = "";
        for (ReservedFunctionContent.FunctionInformation f : funcInfos) {
            sNumOfArgs += String.valueOf(f.numberOfArguments) + " or ";
            if (f.numberOfArguments == numberOfArguments) {
                funcInfo = f; break;
            }
        }

        if (funcInfo == null) { 
            sNumOfArgs = sNumOfArgs.substring(0, sNumOfArgs.length()-4);
            throw new ParseException(
                String.format("Missing arguments %s for `%s` function.", 
                sNumOfArgs, functionName));
        }

        Element parent = (Element)data;
        Element apply   = mDocument.createElement("m:apply");
        parent.appendChild(apply);
        Element func   = mDocument.createElement("m:"+funcInfo.mathmlName);
        apply.appendChild(func);

        // overload function
        if ("log".equals(functionName) && numberOfArguments==2) {
            Element logbase = mDocument.createElement("m:logbase");
            apply.appendChild(logbase);
            node.jjtGetChild(0).jjtAccept(this, logbase);
            node.jjtGetChild(1).jjtAccept(this, apply);
            return data;
        }

        if ("log10".equals(functionName)) {
            Element logbase = mDocument.createElement("m:logbase");
            apply.appendChild(logbase);
            Element cn = mDocument.createElement("m:cn");
            cn.setTextContent("10");
            logbase.appendChild(cn);
            node.jjtGetChild(0).jjtAccept(this, apply);
            return data;
        }

        // overload function
        if ("sqrt".equals(functionName) && numberOfArguments==2) {
            Element sqrt = mDocument.createElement("m:degree");
            apply.appendChild(sqrt);
            node.jjtGetChild(0).jjtAccept(this, sqrt);
            node.jjtGetChild(1).jjtAccept(this, apply);
            return data;
        }

        if ("diff".equals(functionName) && numberOfArguments==2) {
            Element bvar = mDocument.createElement("m:bvar");
            apply.appendChild(bvar);
            node.jjtGetChild(0).jjtAccept(this, bvar);
            node.jjtGetChild(1).jjtAccept(this, apply);
            return data;
        }

        defaultVisit(node, apply);
        return data;
    }

    /*
     * identifier 
     * ex.) variable, a, b, c, x, y ,z ...
     */
    @Override
    public Object visit(ASTIdentifier node, Object data) 
        throws ParseException {
        Element parent = (Element)data;
        Element variable = mDocument.createElement("m:ci");

        mUsingParameterNames.add((String)node.jjtGetValue());
        variable.setTextContent((String)node.jjtGetValue());
        parent.appendChild(variable);
        defaultVisit(node, data);
        return data;
    }

    /*
     * vector
     * ex.) throws ParseException {1}, throws ParseException {1, 2, 3, 4, 5}
     */
    @Override
    public Object visit(ASTVector node, Object data) 
        throws ParseException {
        Element parent = (Element)data;
        if ("VectorList".equals(node.jjtGetParent().toString())) {
            Element matrixrow = mDocument.createElement("m:matrixrow");
            parent.appendChild(matrixrow);
            defaultVisit(node, matrixrow);
            return data;
        } else { // Vector
            Element apply = mDocument.createElement("m:apply");
            parent.appendChild(apply);
            Element vector = mDocument.createElement("m:vector");
            apply.appendChild(vector);
            defaultVisit(node, vector);
            return data;
        }
    }

    /*
     * matrix
     * ex.) throws ParseException {throws ParseException {1,2,3},throws ParseException {4,5,6},throws ParseException {7,8,9}}
     */
    @Override
    public Object visit(ASTMatrix node, Object data) 
        throws ParseException {
        Element parent = (Element)data;
        Element matrix = mDocument.createElement("m:matrix");       
        parent.appendChild(matrix);
        defaultVisit(node, matrix);
        return data;
    }
}