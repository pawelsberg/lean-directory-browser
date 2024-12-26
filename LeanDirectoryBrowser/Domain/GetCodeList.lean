import LeanDirectoryBrowser.Domain.ProgStates.Start
import LeanDirectoryBrowser.Domain.ProgStates.FirstDirectoryLoaded
import LeanDirectoryBrowser.Domain.ProgStates.WidthProvided
import LeanDirectoryBrowser.Domain.ProgStates.HeightProvided
import LeanDirectoryBrowser.Domain.ProgStates.EmptyFolderBrowser
import LeanDirectoryBrowser.Domain.ProgStates.FolderBrowser
import LeanDirectoryBrowser.Domain.ProgStates.ChangingDirectory
import LeanDirectoryBrowser.Domain.ProgStates.Help
import LeanDirectoryBrowser.Domain.ProgStates.Error
import LeanDirectoryBrowser.Domain.ProgStates.Exit

def getCodeList (ps : ProgState) : List Code :=
  match ps with
  | ProgState.start _ =>
    getStartCodeList
  | ProgState.firstDirectoryLoaded _ _ =>
    getFirstDirectoryLoadedCodeList
  | ProgState.widthProvided _ _ _ =>
    getWidthProvidedCodeList
  | ProgState.heightProvided root hRootIsDir currentDirectory hCurrentDirectoryIsDir displayWidth displayHeight displayRows =>
    getHeightProvidedCodeList (ProgState.heightProvided root hRootIsDir currentDirectory hCurrentDirectoryIsDir displayWidth displayHeight displayRows) True.intro
  | ProgState.emptyFolderBrowser root hRootIsDir currentDirectory hCurrentDirectoryIsLoadedEmptyDir displayWidth displayHeight displayRows runPowerShell =>
    getEmptyFolderBrowserCodeList (ProgState.emptyFolderBrowser root hRootIsDir currentDirectory hCurrentDirectoryIsLoadedEmptyDir displayWidth displayHeight displayRows runPowerShell) True.intro
  | ProgState.folderBrowser root hRootIsDir currentDirectory hCurrentDirectoryIsNonEmptyDirectory displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath runPowerShell =>
    getFolderBrowserCodeList (ProgState.folderBrowser root hRootIsDir currentDirectory hCurrentDirectoryIsNonEmptyDirectory displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath runPowerShell) True.intro
  | ProgState.changingDirectory root hRootIsDir currentDirectory hCurrentDirectoryIsDir displayWidth displayHeight displayRows =>
    getChangingDirectoryCodeList (ProgState.changingDirectory root hRootIsDir currentDirectory hCurrentDirectoryIsDir displayWidth displayHeight displayRows) True.intro
  | ProgState.help nps =>
    getHelpCodeList (ProgState.help nps) True.intro
  | ProgState.error nextState errorMessage =>
    getErrorCodeList (ProgState.error nextState errorMessage) True.intro
  | ProgState.exit =>
    getExitCodeList
