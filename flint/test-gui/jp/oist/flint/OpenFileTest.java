/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */

package jp.oist.flint;

import java.awt.Component;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import javax.swing.SwingUtilities;
import jp.oist.flint.command.Main;
import jp.oist.flint.filesystem.Workspace;
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


public class OpenFileTest {

    // file is in test/models
    static final String MODEL_FILENAME = "Izhikevich_2003_model.isml";

    /*
     *  Windows reserved characters
     *      < > : " / \ | ? *
     *  see http://msdn.microsoft.com/en-us/library/aa365247%28VS.85%29#naming_conventions
     */
    static final String[] FILE_NAME_FOR_WINDOWS = new String[] {
        "イジケビッチ ２００３ モデル.isml",
        "イジケビッチ ^(２００３)@モデル.isml",
        "イジケビッチ$ [２００３]%モデル.isml",
        "イジケビッチ {２００３ } ~モデル.isml",
        "イジケビッチ = '２００３'-モデル.isml",
        "イジケビッチ &２００３+モデル.isml",
        "イジケビッチ #２００３%モデル.isml",
        "イジケビッチ_２００３ !モデル.isml",
        "イジケビッチ, ２００３;モデル.isml",
        "%イジケビッチ% ２００３モデル",
        "$イジケビッチ$ ２００３モデル",
        "#イジケビッチ# ２００３モデル"
    };

    /*
     *  Unix reserved characters
     *      / '\0'
     */
    static final String[] FILE_NAME_FOR_UNIX = new String[] {
        "イジケビッチ ２００３ モデル.isml",
        "イジケビッチ ２００３ ?\\|モデル.isml",
        "イジケビッチ ^(２００３)@モデル.isml",
        "イジケビッチ$ [２００３]%モデル.isml",
        "イジケビッチ {２００３} ~モデル.isml",
        "イジケビッチ <２００３> ~モデル.isml",
        "イジケビッチ = '２００３'-モデル.isml",
        "イジケビッチ = \"２００３\"-モデル.isml",
        "イジケビッチ &２００３+モデル.isml",
        "イジケビッチ #２００３%モデル.isml",
        "イジケビッチ_２００３ !モデル.isml",
        "イジケビッチ, ２００３;モデル.isml",
        "イジケビッチ:２００３:モデル.isml",
        "%イジケビッチ% ２００３モデル",
        "$イジケビッチ$ ２００３モデル",
        "#イジケビッチ# ２００３モデル"

    };

    static final String[] DIRECOTRY_NAMES = new String[] {
        "サンプル　モデル (２００３)",
        "サンプル　モデル [２００３]",
        "サンプル　モデル@２００３+",
        "サンプル　モデル@２００３",
        "サンプル・モデル$２００３ ",
        "サンプル　モデル_２００３",
        "サンプル　モデル - ２００３",
        "サンプル　モデル =２００３=",
        "サンプル　モデル '２００３'",
        "サンプル　モデル.２００３",
        "サンプル　モデル, ２００３ ",
        "サンプル・モデル;　２００３ ",
        "%サンプル・モデル%　２００３ ",
        "サンプル・モデル$　２００３ ",
        "サンプル・モデル;　２００３ "
    };

    static String[] mFileNames;

    static String[] mDirectoryNames;

    static File mSampleFile;

    File[] mTestFiles = null;

    FrameFixture mWindow;

    public OpenFileTest() {
    }

    @BeforeClass
    public static void setUpClass() {
        FailOnThreadViolationRepaintManager.install();

        String osName = System.getProperty("os.name");
        if (osName.startsWith("Windows")) {
            mFileNames = FILE_NAME_FOR_WINDOWS;
        } else {
            mFileNames = FILE_NAME_FOR_UNIX;
        }
        mDirectoryNames = DIRECOTRY_NAMES;
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() throws IOException {
        String modelPath = "test" + File.separator + "models"  + File.separator + MODEL_FILENAME;

        File projectDir = new File(System.getProperty("user.dir")).getParentFile();
        mSampleFile = new File(projectDir, modelPath);

        GuiActionRunner.executeInEDT(false);
        MainFrame mainFrame = GuiActionRunner.execute(new GuiQuery<MainFrame>() {
            @Override
            protected MainFrame executeInEDT() throws Throwable {
                return new Main(new String[] {}).getMainFrame();
            }
        });
        mWindow = new FrameFixture(mainFrame);

        int trialCount = 5;
        mTestFiles = new File[trialCount];

        ArrayList<Integer> fileIndexes = new ArrayList();
        for (int i=0; i<trialCount; i++)
            fileIndexes.add(i);
        Collections.shuffle(fileIndexes);

        ArrayList<Integer> dirIndexes = new ArrayList();
        for (int i=0; i<trialCount; i++)
            dirIndexes.add(i);
        Collections.shuffle(dirIndexes);

        for (int i=0; i<trialCount; i++) {
            String filename = mFileNames[fileIndexes.get(i)];
            String dirname  = mDirectoryNames[dirIndexes.get(i)];
            mTestFiles[i] = createTestFile(filename, dirname);
        }
    }

    @After
    public void tearDown() {
        mWindow.cleanUp();
        for (File testFile : mTestFiles)
            Workspace.recursiveDelete(testFile.getParentFile());
    }

    File createTestFile(String filename) throws IOException {
        return createTestFile(filename, "");
    }

    File createTestFile(String filename, String directory) throws IOException {
        Path dirPath = Files.createTempDirectory(directory);
        Path filePath = dirPath.resolve(filename);
        Files.copy(mSampleFile.toPath(), filePath, StandardCopyOption.REPLACE_EXISTING);
        return filePath.toFile();
    }

    @Test
    public void testOpenModelWhichContainsSpaceAndJapanese () {

        for (File testFile : mTestFiles) {
            mWindow.menuItem(MenuBar.OPEN).click();
            JFileChooserFixture filechooser  = JFileChooserFinder.findFileChooser().using(mWindow.robot);
            filechooser.fileNameTextBox().enterText(testFile.getAbsolutePath());
            filechooser.approve();

            Pause.pause(new Condition("Waiting for load model"){
                @Override
                public boolean test() {
                    Collection<Component> subFrames = mWindow.robot.finder().findAll(mWindow.target, new TypeMatcher(SubFrame.class));
                    return subFrames.size() == 1;
                }
            }, 10 * 1000);

            Pause.pause(new Condition("Waiting for control panel"){
                @Override
                public boolean test() {
                    return mWindow.robot.finder().findByName(ControlPane.RUN).isEnabled();
                }
            }, 20 * 1000);
            mWindow.button(ControlPane.RUN).click();

            JOptionPaneFixture optionPane = JOptionPaneFinder.findOptionPane().withTimeout(120 * 1000).using(mWindow.robot);
            optionPane.okButton().click();
            Collection<Component> findAll = mWindow.robot.finder().findAll(mWindow.target,
                    new TypeMatcher(SubFrame.class));

            for (Component c : findAll) {
                final SubFrame subFrame = (SubFrame) c;
                // FIXME
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        subFrame.dispose();
                    }
                });
            }
        }
    }
}
