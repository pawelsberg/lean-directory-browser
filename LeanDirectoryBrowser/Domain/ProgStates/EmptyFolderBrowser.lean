import LeanDirectoryBrowser.Domain.ProgState

def getEmptyFolderBrowserCodeList (ps : ProgState) (_: isProgStateEmptyFolderBrowser ps) : List Code :=
  match ps with
  | ProgState.emptyFolderBrowser _ _ currentDirectory _ _ _ _ _ =>
    [
      Code.clearToColor AllegroColor.black,
      Code.drawStoredFontStr DisplayConstants.displayHeaderFontColour DisplayConstants.displayTopHorizontalMargin DisplayConstants.displayTopVerticalMargin DisplayConstants.displayHeaderFontStorageName FontAlignFlags.left currentDirectory.path,
      Code.run
    ]


def processEmptyFolderBrowser (ps : ProgState) (_ : isProgStateEmptyFolderBrowser ps) (input : String) : ProgState :=
  match ps with
    | ProgState.emptyFolderBrowser root hRootIsDir currentDirectory hCurrDirIsLoadedEmptyDir displayWidth displayHeight displayRows _ =>
      match input with
      | "KEY_DOWN:KeyBackspace" =>
          match currentDirectory with
          | File.directory _ _ =>
            let parentPath := currentDirectory.parentPath
            let parentDirectory := root.findDirectory parentPath
            match parentDirectory, displayRows with
            | some (File.directory _ (some children)), displayRows =>
              let newCurrentDirectory := File.directory parentPath (some children)
              ProgState.heightProvided root hRootIsDir newCurrentDirectory True.intro displayWidth displayHeight displayRows
            | _, _ => ps -- shouldn't happen parent directory should always be a directory
      | "KEY_DOWN:KeyP" => ProgState.emptyFolderBrowser root hRootIsDir currentDirectory hCurrDirIsLoadedEmptyDir displayWidth displayHeight displayRows true
      | "KEY_DOWN:KeyH" => ProgState.help ps
      | "KEY_DOWN:KeyQ" => ProgState.exit
      | "DONE." => ProgState.exit
      | _ => ps -- only process KEY_DOWN:KeyBackspace, KEY_DOWN:KeyQ and DONE


def withPowerShellStartedInEmptyFolderBrowser (cpp : CodeProxyProcess) (ps : ProgState) (_: isProgStateEmptyFolderBrowser ps) : IO ProgState := do
    match ps with
    | ProgState.emptyFolderBrowser root hRootIsDir currentDirectory hCurrentDirIsLoadedEmptyDir displayWidth displayHeight displayRows true => do
      cpp.writeCode (Code.startPowerShell currentDirectory.path)
      cpp.writeCode Code.run
      cpp.flush
      pure (ProgState.emptyFolderBrowser root hRootIsDir currentDirectory hCurrentDirIsLoadedEmptyDir displayWidth displayHeight displayRows false)
    | ProgState.emptyFolderBrowser _ _ _ _ _ _ _ false => pure ps
