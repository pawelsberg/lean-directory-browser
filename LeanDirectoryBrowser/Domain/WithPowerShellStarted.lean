import LeanDirectoryBrowser.Domain.ProgStates.EmptyFolderBrowser
import LeanDirectoryBrowser.Domain.ProgStates.FolderBrowser
import LeanDirectoryBrowser.Allegro

namespace ProgState

  def withPowerShellStarted (cpp : CodeProxyProcess) (ps : ProgState) : IO ProgState := do
    match ps with
    | emptyFolderBrowser root hRootIsDir currentDirectory hCurrentDirIsLoadedEmptyDir displayWidth displayHeight displayRows true => do
      withPowerShellStartedInEmptyFolderBrowser cpp (emptyFolderBrowser root hRootIsDir currentDirectory hCurrentDirIsLoadedEmptyDir displayWidth displayHeight displayRows true) True.intro
    | folderBrowser root hRootIsDir currentDirectory hCurrentDirIsNonEmptyDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath true =>
      withPowerShellStartedInFolderBrowser cpp (folderBrowser root hRootIsDir currentDirectory hCurrentDirIsNonEmptyDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath true) True.intro
    | _ => pure ps -- other states do not support starting PowerShell

end ProgState
