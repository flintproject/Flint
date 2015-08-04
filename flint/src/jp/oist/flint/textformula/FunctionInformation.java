/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint.textformula;

public class FunctionInformation {

	public final String functionName;

	public final String mathmlName;

	public final int numberOfArguments;

	public FunctionInformation(String sn, String mn, int n) {
		this.functionName = sn;
		this.mathmlName = mn;
		this.numberOfArguments = n;
	}
}
