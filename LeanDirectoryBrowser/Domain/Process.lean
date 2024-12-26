import LeanDirectoryBrowser.Domain.ProgStates.Start
import LeanDirectoryBrowser.Domain.ProgStates.FirstDirectoryLoaded
import LeanDirectoryBrowser.Domain.ProgStates.WidthProvided
import LeanDirectoryBrowser.Domain.ProgStates.HeightProvided
import LeanDirectoryBrowser.Domain.ProgStates.EmptyFolderBrowser
import LeanDirectoryBrowser.Domain.ProgStates.FolderBrowser
import LeanDirectoryBrowser.Domain.ProgStates.ChangingDirectory
import LeanDirectoryBrowser.Domain.ProgStates.Help
import LeanDirectoryBrowser.Domain.ProgStates.Error

namespace ProgState

  def process (ps : ProgState) (input : String) : ProgState :=
    match ps with
    | start _ => ps -- doesn't process any input
    | firstDirectoryLoaded root hRootIsDir =>
      processFirstDirectoryLoaded (firstDirectoryLoaded root hRootIsDir) True.intro input
    | widthProvided root hRootIsDir displayWidth =>
      processWidthProvided (widthProvided root hRootIsDir displayWidth) True.intro input
    | heightProvided root hRootIsDir currentDirectory hCurrentDirIsDir displayWidth displayHeight displayRows =>
      processHeightProvided (heightProvided root hRootIsDir currentDirectory hCurrentDirIsDir displayWidth displayHeight displayRows) True.intro input
    | emptyFolderBrowser root hRootIsDir currentDirectory hCurrDirIsLoadedEmptyDir displayWidth displayHeight displayRows _ =>
      processEmptyFolderBrowser (emptyFolderBrowser root hRootIsDir currentDirectory hCurrDirIsLoadedEmptyDir displayWidth displayHeight displayRows False) True.intro input
    | folderBrowser root hRootIsDir currentDirectory hCurrentDirIsNonEmptyDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath runPowerShell =>
      processFolderBrowser (folderBrowser root hRootIsDir currentDirectory hCurrentDirIsNonEmptyDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath runPowerShell) True.intro input
    | changingDirectory root hRootIsDir currentDirectory hCurrentDirIsDir displayWidth displayHeight displayRows =>
      processChangingDirectory (changingDirectory root hRootIsDir currentDirectory hCurrentDirIsDir displayWidth displayHeight displayRows) True.intro input
    | help nextState =>
      processHelp (help nextState) True.intro input
    | error nextState errorMessage =>
      processError (error nextState errorMessage) True.intro input
    | exit => ps -- doesn't process any input

end ProgState
