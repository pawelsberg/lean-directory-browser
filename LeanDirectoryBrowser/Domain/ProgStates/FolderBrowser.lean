import LeanDirectoryBrowser.Domain.ProgState

namespace FolderBrowser
  def getCodeList (ps : ProgState) (_: isProgStateFolderBrowser ps) : List Code :=
    match ps with
    | ProgState.folderBrowser root hRootIsDir currentDirectory _ displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath _ =>
      let draw_file : File → Nat → Nat → Code
      | file, x, y =>
        let filename := file.filename
        let color := if selectedFilePath == file.path then
                      DisplayConstants.displayFileSelectedFontColour
                    else
                      DisplayConstants.displayFileDeselectedFontColour
        Code.drawStoredFontStr color x y DisplayConstants.displayFileFontStorageName FontAlignFlags.left filename

      let rec draw_children_one_level : List File → Nat → Nat → List Code
        | [], _, _ => []
        | f :: fs, x, y =>
          (
            (draw_file f x y)
            ::
            (draw_children_one_level fs x (y + DisplayConstants.displayFileFontSize))
          )

      match currentDirectory, fileOnTopPath with
        | File.directory _ (some children), fileOnTopPath =>
          let rec draw_children_columns : List File → Nat → Nat → Nat → List Code
            | [], _, _, _ => []
            | fs, n, x, y =>
              let (to_draw, rest) := fs.splitAt displayRows
              [
                draw_children_one_level to_draw x y,
                if (n > 1) then
                  draw_children_columns rest (n - 1) (x + displayColumnWidth) y
                else
                  []
              ].join

          let childrenFromFileOnTop := children.drop (File.indexOfFile children fileOnTopPath)
          [
            [
              Code.clearToColor AllegroColor.black,
              Code.drawStoredFontStr DisplayConstants.displayHeaderFontColour DisplayConstants.displayTopHorizontalMargin DisplayConstants.displayTopVerticalMargin DisplayConstants.displayHeaderFontStorageName FontAlignFlags.left currentDirectory.path
            ],
            draw_children_columns childrenFromFileOnTop displayColumns 0 (DisplayConstants.displayTopVerticalMargin + DisplayConstants.displayHeaderFontSize + DisplayConstants.displayHeaderMargin),
            [Code.run]
          ].join

  def processMovingSelectionForward (ps : ProgState) (_ : isProgStateFolderBrowser ps) (positionsToMove : Nat) : ProgState :=
    match ps with
    | ProgState.folderBrowser root hRootIsDir currentDirectory hCurrDirIsDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath runPowerShell =>
        let hcd := hCurrDirIsDir
        match currentDirectory with
        | File.directory _ (some children) =>
        let newSelectedFilePath := ((List.range positionsToMove).map toString).foldl (λ currentFilePath _ => File.findNextFilePath children currentFilePath) selectedFilePath
        let moveRightColumns : Nat := ((File.indexOfFile children newSelectedFilePath) - (File.indexOfFile children fileOnTopPath)) / displayRows + 1 - displayColumns
        let newFileOnTopPath := if moveRightColumns > 0
          then
            let newFileOnTopIndex := (File.indexOfFile children fileOnTopPath) + displayRows * moveRightColumns
            match (children.drop newFileOnTopIndex) with
            | [] => fileOnTopPath -- shouldn't happen
            | f :: _ => f.path
          else fileOnTopPath
        ProgState.folderBrowser root hRootIsDir currentDirectory hcd displayWidth displayHeight displayRows displayColumns displayColumnWidth newSelectedFilePath newFileOnTopPath runPowerShell

  def processMovingSelectionBackward (ps : ProgState) (_ : isProgStateFolderBrowser ps) (positionsToMove : Nat) : ProgState :=
    match ps with
    | ProgState.folderBrowser root hRootIsDir currentDirectory hCurrentDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath runPowerShell =>
      let hcd := hCurrentDir
      match currentDirectory with
      | File.directory _ (some children) =>
        let newSelectedFilePath := ((List.range positionsToMove).map toString).foldl (λ currentFilePath _ => File.findPreviousFilePath children currentFilePath) selectedFilePath
        let moveLeftColumns : Nat := ((File.indexOfFile children fileOnTopPath) - (File.indexOfFile children newSelectedFilePath) + (displayRows - 1) ) / displayRows
        let newFileOnTopPath := if moveLeftColumns > 0
          then
            let newFileOnTopIndex := (File.indexOfFile children fileOnTopPath) - displayRows * moveLeftColumns
            match (children.drop newFileOnTopIndex) with
            | [] => fileOnTopPath -- shouldn't happen
            | f :: _ => f.path
          else fileOnTopPath
        ProgState.folderBrowser root hRootIsDir currentDirectory hcd displayWidth displayHeight displayRows displayColumns displayColumnWidth newSelectedFilePath newFileOnTopPath runPowerShell



  def process (ps : ProgState) (_ : isProgStateFolderBrowser ps) (input : String) : ProgState :=
    match h : ps with
      | ProgState.folderBrowser root hRootIsDir currentDirectory hCurrentDirIsNonEmptyDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath runPowerShell =>
        let psIsFolderBrowser : isProgStateFolderBrowser ps := (by simp [h, isProgStateFolderBrowser])
        match input with
        | "KEY_DOWN:KeyDown" => processMovingSelectionForward ps psIsFolderBrowser 1
        | "KEY_DOWN:KeyRight" => processMovingSelectionForward ps psIsFolderBrowser displayRows
        | "KEY_DOWN:KeyPageDown" => processMovingSelectionForward ps psIsFolderBrowser (displayRows * displayColumns)
        | "KEY_DOWN:KeyEnd" => match currentDirectory with
          | File.directory _ (some children) => processMovingSelectionForward ps psIsFolderBrowser children.length
          | _ => ps
        | "KEY_DOWN:KeyUp" => processMovingSelectionBackward ps psIsFolderBrowser 1
        | "KEY_DOWN:KeyLeft" => processMovingSelectionBackward ps psIsFolderBrowser displayRows
        | "KEY_DOWN:KeyPageUp" => processMovingSelectionBackward ps psIsFolderBrowser (displayRows * displayColumns)
        | "KEY_DOWN:KeyHome" => match currentDirectory with
          | File.directory _ (some children) => processMovingSelectionBackward ps psIsFolderBrowser children.length
          | _ => ps
        | "KEY_DOWN:KeyEnter" =>
            match currentDirectory with
            | File.directory _ (some children) =>
              match children.find? (λ f => f.path == selectedFilePath) with
              | some (File.directory path children) =>
                ProgState.heightProvided root hRootIsDir (File.directory path children) True.intro displayWidth displayHeight displayRows
              | _ => ps -- selected file is not a directory - do nothing
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
        | "KEY_DOWN:KeyP" => ProgState.folderBrowser root hRootIsDir currentDirectory hCurrentDirIsNonEmptyDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath true
        | "KEY_DOWN:KeyH" => ProgState.help ps
        | "KEY_DOWN:KeyQ" => ProgState.exit
        | "DONE." => ProgState.exit
        | _ => ps



  def withPowerShellStarted (cpp : CodeProxyProcess) (ps : ProgState) (_: isProgStateFolderBrowser ps) : IO ProgState := do
      match ps with
      | ProgState.folderBrowser root hRootIsDir currentDirectory hCurrentDirIsNonEmptyDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath true =>
        cpp.writeCode (Code.startPowerShell currentDirectory.path)
        cpp.writeCode Code.run
        cpp.flush
        pure (ProgState.folderBrowser root hRootIsDir currentDirectory hCurrentDirIsNonEmptyDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath false)
      | ProgState.folderBrowser _ _ _ _ _ _ _ _ _ _ _ false => pure ps
end FolderBrowser
