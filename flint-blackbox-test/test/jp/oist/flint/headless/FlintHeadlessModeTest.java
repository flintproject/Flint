/* -*- Mode: Java; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et: */

package jp.oist.flint.headless;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

import jp.oist.flint.util.FileUtility;
import jp.oist.flint.util.ProcessWatcher;

public class FlintHeadlessModeTest {

    // file is in test/models 
    static final String MODEL_FILENAME = "Izhikevich_2003_model.isml";

    /* 
     *  Windows reserved characters 
     *      < > : " / \ | ? *
     *  see http://msdn.microsoft.com/en-us/library/aa365247%28VS.85%29#naming_conventions  
     */
    static final String[] FILE_NAME_FOR_WINDOWS = new String[] {
        "イジケビッチ ２００３ モデル",
        "イジケビッチ ^(２００３)@モデル",
        "イジケビッチ$ [２００３]%モデル",
        "イジケビッチ {２００３ } ~モデル",
        "イジケビッチ = '２００３'-モデル",
        "イジケビッチ &２００３+モデル",
        "イジケビッチ #２００３%モデル",
        "イジケビッチ_２００３ !モデル",
        "イジケビッチ, ２００３;モデル",
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
        "イジケビッチ:２００３:モデル.isml"
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
    };

    static String mExtension;

    static String[] mFileNames;

    static String[] mDirectoryNames;

    static File mSampleFile;

    static String FLINT_PATH;

    File[] mTestFiles;

    File[] mOutputFiles;

    File mTemporaryDir = null;

    static boolean mIsWindows = false;
    
    public FlintHeadlessModeTest() {
    }
    
    @BeforeClass
    public static void setUpClass() {
        String osName = System.getProperty("os.name");
        if (osName.startsWith("Windows")) {
            mFileNames = FILE_NAME_FOR_WINDOWS;
            FLINT_PATH = "C:\\Program Files (x86)\\Flint";
            mIsWindows = true;
        } else if (osName.startsWith("Mac OS X")) { 
            mFileNames = FILE_NAME_FOR_UNIX;
        } else {
            mFileNames = FILE_NAME_FOR_UNIX;
        }
        mDirectoryNames = DIRECOTRY_NAMES;

        File projectDir = new File(System.getProperty("user.dir")).getParentFile();
        File modelDir = new File(projectDir, "test" + File.separator + "models" + File.separator);

        mSampleFile = new File(modelDir, MODEL_FILENAME);
    }
    
    @AfterClass
    public static void tearDownClass() {
    }
    
    @Before
    public void setUp() throws IOException {
        ArrayList<Integer> filenameIndexes = new ArrayList<Integer>();
        for (int i=0; i<mFileNames.length; i++)
            filenameIndexes.add(i);
        Collections.shuffle(filenameIndexes);

        ArrayList<Integer> dirnameIndexes = new ArrayList<Integer>();
        for (int i=0; i<mDirectoryNames.length; i++) 
            dirnameIndexes.add(i);
        Collections.shuffle(dirnameIndexes);

        int trialCount = 5;
        mTestFiles = new File[trialCount];
        for (int i=0; i<trialCount; i++) {
            mTestFiles[i] = createTestFile(
                    mFileNames[filenameIndexes.get(i)],
                    mDirectoryNames[dirnameIndexes.get(i)]
            );
        }
    }
    
    @After
    public void tearDown() {
        for (File f : mTestFiles)
            FileUtility.recursiveDelete(f.getParentFile());
        mTestFiles = null;
    }

    File createTestFile (String filename) throws IOException {
        return createTestFile(filename, "");
    }

    File createTestFile (String filename, String directory) throws IOException {
        Path dirPath = Files.createTempDirectory(directory);
            String ext = FileUtility.extension(mSampleFile);
            Path filePath = dirPath.resolve(filename + "." + ext);
            Files.copy(mSampleFile.toPath(), filePath);

            return filePath.toFile();
    }

    public ArrayList<String> getShell() {
        ArrayList<String> command = new ArrayList<String>();
        if (mIsWindows) {
            command.add("cmd");
            command.add("/c");
        } else {
            command.add("/bin/sh");
            command.add("-c");
        }
        return command;
    }

    public ProcessBuilder makeCommand (File inputFile, File outputFile) {
        ArrayList<String> command = getShell();
        command.add("flint");
        command.add("-headless");
        command.add(inputFile.getAbsolutePath());
        command.add(outputFile.getAbsolutePath());
        ProcessBuilder pb = new ProcessBuilder(command);
        if (FLINT_PATH != null) {
            // On Windows, current directory is included for searching commands.
            pb.directory(new File(FLINT_PATH));
        }
        pb.redirectErrorStream(true);

        return pb;
    }

    @Test
    public void testOpenModelWhichContainsSpaceAndJapanese() throws IOException, InterruptedException {
        for (File inputFile : mTestFiles) {
            File outputFile= File.createTempFile(inputFile.getName(), ".isd");
                outputFile.delete();

                ProcessBuilder command = makeCommand(inputFile, outputFile);
                Process process = command.start();
                ProcessWatcher watcher = new ProcessWatcher(process.getInputStream()) {
                    @Override
                    protected void process (List<String> lines) {
                        for (String line : lines)
                            System.err.println(line);
                    }
                };
                watcher.execute();
                int status = process.waitFor();
                assertEquals(0, status);
                assertTrue(outputFile.exists());
                outputFile.delete();
        }
    }
}
