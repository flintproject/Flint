/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint;

import java.awt.Component;
import java.io.File;
import java.util.Collection;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import jp.oist.flint.command.Main;
import jp.oist.flint.form.FlintMenuBar;
import jp.oist.flint.form.MainFrame;
import jp.oist.flint.form.sub.ParameterValuePane;
import jp.oist.flint.form.sub.SubFrame;
import jp.oist.flint.form.sub.ValueEditWindow;
import org.fest.swing.core.TypeMatcher;
import org.fest.swing.edt.GuiActionRunner;
import org.fest.swing.edt.GuiQuery;
import org.fest.swing.finder.FrameFinder;
import org.fest.swing.finder.JFileChooserFinder;
import org.fest.swing.finder.JOptionPaneFinder;
import org.fest.swing.finder.WindowFinder;
import org.fest.swing.fixture.FrameFixture;
import org.fest.swing.fixture.JButtonFixture;
import org.fest.swing.fixture.JFileChooserFixture;
import org.fest.swing.fixture.JOptionPaneFixture;
import org.fest.swing.fixture.JPanelFixture;
import org.fest.swing.fixture.JTabbedPaneFixture;
import org.fest.swing.fixture.JTextComponentFixture;
import org.fest.swing.timing.Condition;
import org.fest.swing.timing.Pause;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import static org.junit.Assert.*;

public class InputValueSetTest {

    // file is in test/models
    static final String MODEL_FILENAME = "Izhikevich_2003_model.isml";

    static MainFrame mMainFrame;
    static FrameFixture mWindow;
    static JButtonFixture mDefineValueSetButton;

    FrameFixture mValueEditWindow;

    public InputValueSetTest() {
    }

    @BeforeClass
    public static void setUpClass() {
        GuiActionRunner.executeInEDT(false);
        mMainFrame = GuiActionRunner.execute(new GuiQuery<MainFrame>() {
            @Override
            protected MainFrame executeInEDT() throws Throwable {
                return new Main(new String[] {}).getMainFrame();
            }
        });
        mWindow = new FrameFixture(mMainFrame);
        GuiActionRunner.executeInEDT(true);
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
        File projectDir = new File(System.getProperty("user.dir")).getParentFile();
        String modelPath = "test" + File.separator + "models"  + File.separator + MODEL_FILENAME;
        File testFile = new File(projectDir, modelPath);

        mWindow.menuItem(FlintMenuBar.OPEN).click();
        JFileChooserFixture filechooser  = JFileChooserFinder.findFileChooser().using(mWindow.robot);
        filechooser.fileNameTextBox().setText(testFile.getAbsolutePath());
        filechooser.approve();

        Pause.pause(new Condition("Waiting for load model"){
            @Override
            public boolean test() {
                Collection<Component> subFrames = mWindow.robot.finder().findAll(mWindow.target, new TypeMatcher(SubFrame.class));
                return subFrames.size() == 1;
            }
        }, 10 * 1000);

        SubFrame subFrame = (SubFrame)mWindow.robot.finder().find(mWindow.target, new TypeMatcher(SubFrame.class));
        JTabbedPane tabbedPane = (JTabbedPane)subFrame.getContentPane();

        JTabbedPaneFixture tabbedPaneFixture = new JTabbedPaneFixture(mWindow.robot, tabbedPane);
        tabbedPaneFixture.selectTab("Parameters");

        JPanel parameterValuePane = (JPanel)tabbedPane.getSelectedComponent();
        JPanelFixture parameterValuePaneFixutre = new JPanelFixture(mWindow.robot, parameterValuePane);
        mDefineValueSetButton = parameterValuePaneFixutre.button(ParameterValuePane.DEFINE_VALUE_SET_BUTTON);

        mDefineValueSetButton.click();

        FrameFinder frameFinder = WindowFinder.findFrame(ValueEditWindow.class);
        mValueEditWindow = frameFinder.using(mWindow.robot);
        Pause.pause(new Condition("Waiting for ValueEditWindow"){
            @Override
            public boolean test() {
                return mValueEditWindow.target.isVisible();
            }
        }, 10 * 1000);
        JButtonFixture deleteRowButton = mValueEditWindow.button(ValueEditWindow.DELETE_ROW_BUTTON);
        deleteRowButton.click();
    }

    @After
    public void tearDown() {
        mMainFrame.closeAll();
    }

    @Test
    public void testInputRandomizerValue() {
        String inputName = "value1";
        String inputSeed = "1";
        String inputCount = "10";
        String inputLowerLimit = "10";
        String inputUpperLimit = "100";

        String expectedName = inputName;
        String expectedEnumValue = "97,95,94,41,47,46,40,11,29,76";

        mValueEditWindow.checkBox(ValueEditWindow.RANDOMIZER_CHECKBOX).click();

        JTextComponentFixture textNameFixture = mValueEditWindow.textBox(ValueEditWindow.NAME_TEXT);
        JTextComponentFixture textSeedFixture = mValueEditWindow.textBox(ValueEditWindow.RM_SEED_TEXT);
        JTextComponentFixture textCountFixture = mValueEditWindow.textBox(ValueEditWindow.RM_COUNT_TEXT);
        JTextComponentFixture textLowerLimitFixture = mValueEditWindow.textBox(ValueEditWindow.RM_LOWER_LIMIT_TEXT);
        JTextComponentFixture textUpperLimitFixture = mValueEditWindow.textBox(ValueEditWindow.RM_UPPER_LIMIT_TEXT);

        textNameFixture.enterText(inputName);
        textSeedFixture.enterText(inputSeed);
        textCountFixture.enterText(inputCount);
        textLowerLimitFixture.enterText(inputLowerLimit);
        textUpperLimitFixture.enterText(inputUpperLimit);

        mValueEditWindow.button(ValueEditWindow.GENERATE_BUTTON).click();

        JButtonFixture okButton = mValueEditWindow.button(ValueEditWindow.OK_BUTTON);
        okButton.click();

        mDefineValueSetButton.click();

        FrameFinder frameFinder = WindowFinder.findFrame(ValueEditWindow.class);
        final FrameFixture reopenedValueEditWindow = frameFinder.using(mWindow.robot);
        Pause.pause(new Condition("Waiting for ValueEditWindow"){
            @Override
            public boolean test() {
                return reopenedValueEditWindow.target.isVisible();
            }
        }, 10 * 1000);

        reopenedValueEditWindow.textBox(ValueEditWindow.NAME_TEXT).requireText(expectedName);
        reopenedValueEditWindow.textBox(ValueEditWindow.ENUM_VALUE_TEXT).requireText(expectedEnumValue);

        okButton.click();
    }

    @Test
    public void testCancelButton() {
        String inputName = "value1";
        String inputEnumValue = "1,2,3,4,5";

        mValueEditWindow.checkBox(ValueEditWindow.RANDOMIZER_CHECKBOX).click();

        JTextComponentFixture textNameFixture = mValueEditWindow.textBox(ValueEditWindow.NAME_TEXT);
        JTextComponentFixture textAreaEnumFixture = mValueEditWindow.textBox(ValueEditWindow.ENUM_VALUE_TEXT);

        textNameFixture.enterText(inputName);
        textAreaEnumFixture.enterText(inputEnumValue);

        JButtonFixture cancelButton = mValueEditWindow.button(ValueEditWindow.CANCEL_BUTTON);
        cancelButton.click();
    }

    @Test
    public void testInputValidEnumValueWithOkButton () {
        testInputEnumValueWithOkButton("value1", "1,2,3,4,5,6,7",
                                       "value1", "1,2,3,4,5,6,7",
                                       true);
    }

    @Test
    public void testInputValidEnumValueWithApplyButton () {
        testInputEnumValueWithApplyButton("value1", "1,2,3,4,5,6,7",
                                          "value1", "1,2,3,4,5,6,7",
                                          true);
    }

    @Test
    public void testInputInvalidNameWithOkButton () {
        testInputEnumValueWithOkButton("123value", "1,2,3,4,5,6,7",
                                       "",   "",
                                       false);
    }

    @Test
    public void testInputInvalidNameWithApplyButton () {
        testInputEnumValueWithApplyButton("123value", "1,2,3,4,5,6,7",
                                          "",   "",
                                          false);
    }

    @Test
    public void testInputInvalidEnumValueAndOkButton () {
        testInputEnumValueWithOkButton("value3", "1,2,3,4,5,6a",
                                       "", "",
                                       false);
    }

    @Test
    public void testInputInvalidEnumValueAndApplyButton () {
        testInputEnumValueWithApplyButton("value3", "1,2,3,4,5,6a",
                                          "", "",
                                          false);
    }

    @Test
    public void testInputValidIntervalValueWithOkButton () {
        testInputIntervalValueWithOkButton("value1", "0.1", "1.0", "0.1",
                                           "value1", "0.1", "1.0", "0.1",
                                           true);
    }

    @Test
    public void testInputValidIntervalValueWithApplyButton () {
        testInputIntervalValueWithApplyButton("value1", "0.1", "1.0", "0.1",
                                              "value1", "0.1", "1.0", "0.1",
                                              true);
    }

    @Test
    public void testInputInvalidIntervalLowerValueWithOkButton () {
        testInputIntervalValueWithOkButton("value2", "a", "1.0", "0.1",
                                           "", "", "", "",
                                           false);
    }

    @Test
    public void testInputInvalidIntervalLowerValueWithApplyButton () {
        testInputIntervalValueWithApplyButton("value2", "a", "1.0", "0.1",
                                              "", "", "", "",
                                              false);
    }

    @Test
    public void testInputInvalidIntervalUpperValueWithOkButton () {
        testInputIntervalValueWithOkButton("value3", "1", "10abcd", "1",
                                           "", "", "", "",
                                           false);
    }

    @Test
    public void testInputInvalidIntervalUpperValueWithApplyButton () {
        testInputIntervalValueWithApplyButton("value3", "1", "10abcd", "1",
                                              "", "", "", "",
                                              false);
    }

    @Test
    public void testInputInvalidIntervalStepValueWithOkButton () {
        testInputIntervalValueWithOkButton("value3", "0.2", "2", "0.2abcd",
                                           "", "", "", "",
                                           false);
    }

    @Test
    public void testInputInvalidIntervalStepValueWithApplyButton () {
        testInputIntervalValueWithApplyButton("value3", "0.2", "2", "0.2abcd",
                                              "", "", "", "",
                                              false);
    }

    private void testInputEnumValueWithApplyButton (String inputName, String inputEnumValue,
                                        String expectedName, String expectedEnumValue,
                                        boolean isSuccessTest) {
        mValueEditWindow.radioButton(ValueEditWindow.ENUM_RADIO_BUTTON).click();

        JTextComponentFixture textNameFixture = mValueEditWindow.textBox(ValueEditWindow.NAME_TEXT);
        JTextComponentFixture textAreaEnumFixture = mValueEditWindow.textBox(ValueEditWindow.ENUM_VALUE_TEXT);

        textNameFixture.enterText(inputName);
        textAreaEnumFixture.enterText(inputEnumValue);
        mValueEditWindow.button(ValueEditWindow.APPLY_BUTTON).click();

        if (isSuccessTest) {
            mValueEditWindow.button(ValueEditWindow.CANCEL_BUTTON).click();

            mDefineValueSetButton.click();

            FrameFinder frameFinder = WindowFinder.findFrame(ValueEditWindow.class);
            final FrameFixture reopenedValueEditWindow = frameFinder.using(mWindow.robot);
            Pause.pause(new Condition("Waiting for ValueEditWindow"){
                @Override
                public boolean test() {
                    return reopenedValueEditWindow.target.isVisible();
                }
            }, 10 * 1000);

            reopenedValueEditWindow.textBox(ValueEditWindow.NAME_TEXT).requireText(expectedName);
            reopenedValueEditWindow.textBox(ValueEditWindow.ENUM_VALUE_TEXT).requireText(expectedEnumValue);

            mValueEditWindow.button(ValueEditWindow.CANCEL_BUTTON).click();
        } else {
            JOptionPaneFixture optionPaneFixture = JOptionPaneFinder.findOptionPane().using(mWindow.robot);
            optionPaneFixture.okButton().click();
            mValueEditWindow.button(ValueEditWindow.CANCEL_BUTTON).click();
        }
    }

    private void testInputEnumValueWithOkButton (String inputName, String inputEnumValue,
                                        String expectedName, String expectedEnumValue,
                                        boolean isSuccessTest) {
        mValueEditWindow.radioButton(ValueEditWindow.ENUM_RADIO_BUTTON).click();

        JTextComponentFixture textNameFixture = mValueEditWindow.textBox(ValueEditWindow.NAME_TEXT);
        JTextComponentFixture textAreaEnumFixture = mValueEditWindow.textBox(ValueEditWindow.ENUM_VALUE_TEXT);

        textNameFixture.enterText(inputName);
        textAreaEnumFixture.enterText(inputEnumValue);
        JButtonFixture okButton = mValueEditWindow.button(ValueEditWindow.OK_BUTTON);
        okButton.click();

        if (isSuccessTest) {
            mDefineValueSetButton.click();

            FrameFinder frameFinder = WindowFinder.findFrame(ValueEditWindow.class);
            final FrameFixture reopenedValueEditWindow = frameFinder.using(mWindow.robot);
            Pause.pause(new Condition("Waiting for ValueEditWindow"){
                @Override
                public boolean test() {
                    return reopenedValueEditWindow.target.isVisible();
                }
            }, 10 * 1000);

            reopenedValueEditWindow.textBox(ValueEditWindow.NAME_TEXT).requireText(expectedName);
            reopenedValueEditWindow.textBox(ValueEditWindow.ENUM_VALUE_TEXT).requireText(expectedEnumValue);

            okButton.click();
        } else {
            JOptionPaneFixture optionPaneFixture = JOptionPaneFinder.findOptionPane().using(mWindow.robot);
            optionPaneFixture.okButton().click();
            mValueEditWindow.button(ValueEditWindow.CANCEL_BUTTON).click();
        }
    }

    private void testInputIntervalValueWithApplyButton (String inputName, String inputLower, String inputUpper, String inputStep,
                                            String expectedName, String expectedLower, String expectedUpper, String expectedStep,
                                            boolean isSuccessTest) {
        mValueEditWindow.radioButton(ValueEditWindow.INTERVAL_RADIO_BUTTON).click();

        JTextComponentFixture textNameFixture = mValueEditWindow.textBox(ValueEditWindow.NAME_TEXT);
        JTextComponentFixture textRangeLowerFixture = mValueEditWindow.textBox(ValueEditWindow.RANGE_LOWER_TEXT);
        JTextComponentFixture textRangeUpperFixture = mValueEditWindow.textBox(ValueEditWindow.RANGE_UPPER_TEXT);
        JTextComponentFixture textRangeStepFixture = mValueEditWindow.textBox(ValueEditWindow.RANGE_STEP_TEXT);

        textNameFixture.enterText(inputName);
        textRangeLowerFixture.enterText(inputLower);
        textRangeUpperFixture.enterText(inputUpper);
        textRangeStepFixture.enterText(inputStep);

        mValueEditWindow.button(ValueEditWindow.APPLY_BUTTON).click();

        JButtonFixture cancelButton = mValueEditWindow.button(ValueEditWindow.CANCEL_BUTTON);

        if (isSuccessTest) {
            cancelButton.click();

            Pause.pause(new Condition("Waiting for ValueEditWindow"){
                @Override
                public boolean test() {
                    return mValueEditWindow.target.isVisible() == false;
                }
            }, 10 * 1000);

            mDefineValueSetButton.click();

            FrameFinder frameFinder = WindowFinder.findFrame(ValueEditWindow.class);
            final FrameFixture reopenedValueEditWindow = frameFinder.using(mWindow.robot);
            Pause.pause(new Condition("Waiting for ValueEditWindow"){
                @Override
                public boolean test() {
                    return reopenedValueEditWindow.target.isVisible();
                }
            }, 10 * 1000);

            reopenedValueEditWindow.textBox(ValueEditWindow.NAME_TEXT).requireText(expectedName);
            reopenedValueEditWindow.textBox(ValueEditWindow.RANGE_LOWER_TEXT).requireText(expectedLower);
            reopenedValueEditWindow.textBox(ValueEditWindow.RANGE_UPPER_TEXT).requireText(expectedUpper);
            reopenedValueEditWindow.textBox(ValueEditWindow.RANGE_STEP_TEXT).requireText(expectedStep);

            cancelButton.click();
        } else {
            JOptionPaneFixture optionPaneFixture = JOptionPaneFinder.findOptionPane().using(mWindow.robot);
            optionPaneFixture.okButton().click();

            cancelButton.click();
        }
    }

    private void testInputIntervalValueWithOkButton (String inputName, String inputLower, String inputUpper, String inputStep,
                                            String expectedName, String expectedLower, String expectedUpper, String expectedStep,
                                            boolean isSuccessTest) {
        mValueEditWindow.radioButton(ValueEditWindow.INTERVAL_RADIO_BUTTON).click();

        JTextComponentFixture textNameFixture = mValueEditWindow.textBox(ValueEditWindow.NAME_TEXT);
        JTextComponentFixture textRangeLowerFixture = mValueEditWindow.textBox(ValueEditWindow.RANGE_LOWER_TEXT);
        JTextComponentFixture textRangeUpperFixture = mValueEditWindow.textBox(ValueEditWindow.RANGE_UPPER_TEXT);
        JTextComponentFixture textRangeStepFixture = mValueEditWindow.textBox(ValueEditWindow.RANGE_STEP_TEXT);

        textNameFixture.enterText(inputName);
        textRangeLowerFixture.enterText(inputLower);
        textRangeUpperFixture.enterText(inputUpper);
        textRangeStepFixture.enterText(inputStep);

        JButtonFixture okButton = mValueEditWindow.button(ValueEditWindow.OK_BUTTON);
        okButton.click();

        if (isSuccessTest) {
            mDefineValueSetButton.click();

            FrameFinder frameFinder = WindowFinder.findFrame(ValueEditWindow.class);
            final FrameFixture reopenedValueEditWindow = frameFinder.using(mWindow.robot);
            Pause.pause(new Condition("Waiting for ValueEditWindow"){
                @Override
                public boolean test() {
                    return reopenedValueEditWindow.target.isVisible();
                }
            }, 10 * 1000);

            reopenedValueEditWindow.textBox(ValueEditWindow.NAME_TEXT).requireText(expectedName);
            reopenedValueEditWindow.textBox(ValueEditWindow.RANGE_LOWER_TEXT).requireText(expectedLower);
            reopenedValueEditWindow.textBox(ValueEditWindow.RANGE_UPPER_TEXT).requireText(expectedUpper);
            reopenedValueEditWindow.textBox(ValueEditWindow.RANGE_STEP_TEXT).requireText(expectedStep);

            okButton.click();
        } else {
            JOptionPaneFixture optionPaneFixture = JOptionPaneFinder.findOptionPane().using(mWindow.robot);
            optionPaneFixture.okButton().click();

            mValueEditWindow.button(ValueEditWindow.CANCEL_BUTTON).click();
        }
    }
}
