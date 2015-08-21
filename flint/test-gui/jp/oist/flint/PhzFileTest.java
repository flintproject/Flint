/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */
package jp.oist.flint;

import java.awt.Component;
import java.io.File;
import java.io.IOException;
import java.util.Collection;
import jp.oist.flint.command.Main;
import jp.oist.flint.form.ControlPane;
import jp.oist.flint.form.MenuBar;
import jp.oist.flint.form.MainFrame;
import jp.oist.flint.form.sub.SubFrame;
import org.fest.swing.core.TypeMatcher;
import org.fest.swing.edt.FailOnThreadViolationRepaintManager;
import org.fest.swing.edt.GuiActionRunner;
import org.fest.swing.edt.GuiQuery;
import org.fest.swing.finder.JFileChooserFinder;
import org.fest.swing.finder.JOptionPaneFinder;
import org.fest.swing.fixture.FrameFixture;
import org.fest.swing.fixture.JFileChooserFixture;
import org.fest.swing.fixture.JOptionPaneFixture;
import org.fest.swing.timing.Condition;
import org.fest.swing.timing.Pause;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class PhzFileTest {

    static final String FILENAME = "damping_oscillation.phz";

    FrameFixture mWindow;

    @BeforeClass
    public static void setUpClass() {
        FailOnThreadViolationRepaintManager.install();
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() throws IOException {
        GuiActionRunner.executeInEDT(false);
        MainFrame mainFrame = GuiActionRunner.execute(new GuiQuery<MainFrame>() {
            @Override
            protected MainFrame executeInEDT() throws Throwable {
                return new Main(new String[] {}).getMainFrame();
            }
        });
        mWindow = new FrameFixture(mainFrame);
    }

    @After
    public void tearDown() {
        mWindow.cleanUp();
    }

    @Test
    public void testOpenPhzFileAndRun() {
        String modelPath = "test" + File.separator + "models"  + File.separator + FILENAME;
        File rootDir = new File(System.getProperty("user.dir")).getParentFile();
        File file = new File(rootDir, modelPath);

        mWindow.menuItem(MenuBar.OPEN).click();
        JFileChooserFixture filechooser  = JFileChooserFinder.findFileChooser().using(mWindow.robot);
        filechooser.fileNameTextBox().enterText(file.getAbsolutePath());
        filechooser.approve();

        Pause.pause(new Condition("Waiting for subframe") {
            @Override
            public boolean test() {
                Collection<Component> subFrames = mWindow.robot.finder().findAll(mWindow.target, new TypeMatcher(SubFrame.class));
                return subFrames.size() == 1;
            }
        }, 10 * 1000);

        Pause.pause(new Condition("Waiting for control panel") {
            @Override
            public boolean test() {
                return mWindow.robot.finder().findByName(ControlPane.RUN).isEnabled();
            }
        }, 20 * 1000);
        mWindow.button(ControlPane.RUN).click();

        JOptionPaneFixture optionPane = JOptionPaneFinder.findOptionPane().withTimeout(30 * 1000).using(mWindow.robot);
        optionPane.okButton().click();
    }
}
