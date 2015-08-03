/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.textformula.mathml;

import jp.oist.flint.textformula.ReservedFunctionContent;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.io.IOException;

public class MathMLHandler {

	private FormulaNode root=null, cur;

	public void parse(Document doc) {
		NodeList nodeList = doc.getElementsByTagNameNS(MathML2TextFormula.MATHML_NAMESPACE_URI, "math");

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
            switch (localName) {
            case "log":
                {
                    Element child = searchChild((Element)element.getParentNode(), "logbase");
                    if (child != null) {
                        FormulaNode childNode = new ValueOrVariableFormulaNode("logbase");
                        childNode.value = child.getFirstChild().getTextContent().trim();
                        node.addChildNode(childNode);
                    }
                }
                break;
            case "root":
                {
                    Element child = searchChild((Element)element.getParentNode(), "degree");
                    if (child != null) {
                        FormulaNode childNode = new ValueOrVariableFormulaNode("degree");
                        childNode.value = child.getFirstChild().getTextContent().trim();
                        node.addChildNode(childNode);
                    }
                }
                break;
            case "fn":
                node = new CustomFunctionFormulaNode(element.getTextContent().trim());
                break;
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

			if ("matrixrow".equals(child.getLocalName())
				|| "matrix".equals(child.getLocalName())) {
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
		return "logbase".equals(localName) || "degree".equals(localName);
	}

	public boolean isAdditiveTag (String localName) {
		return "plus".equals(localName) || "minus".equals(localName);
	}

	public boolean isMultiveTag (String localName) {
		return "times".equals(localName) || "divide".equals(localName);
	}

	public boolean isEqualityTag (String localName) {
		return "neq".equals(localName) || "eq".equals(localName);
	}

	public boolean isLogicalTag (String localName) {
		return "and".equals(localName) || "or".equals(localName);
	}

	public boolean isRelationalTag (String localName) {
		return "gt".equals(localName) || "lt".equals(localName)
			|| "geq".equals(localName) || "leq".equals(localName);
	}

	public boolean isValueOrVariableTag (String localName) {
		return "cn".equals(localName) || "ci".equals(localName);
	}

	public boolean isFunctionTag (String localName) {
		return (ReservedFunctionContent.indexOfMathmlName(localName) >= 0 ||
				"fn".equals(localName));
	}

	public boolean isVectorTag (String localName) {
		return "vector".equals(localName);
	}

	public boolean isMatrixTag (String localName) {
		return "matrix".equals(localName);
	}

	public boolean isMatrixRowTag (String localName) {
		return "matrixrow".equals(localName);
	}
}
