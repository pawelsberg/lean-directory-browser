import LeanDirectoryBrowser.ProgState.ProgState
import LeanDirectoryBrowser.File
import LeanDirectoryBrowser.ProgState.DisplayConstants
import LeanDirectoryBrowser.Allegro

namespace ProgState

  def withLoadedChildren (ps : ProgState) : IO ProgState :=
    match ps with
    | start rootDirectoryPath => do
      let children ← File.readChildren rootDirectoryPath
      let sortedChildren := File.sortFiles children
      let currentDirectory := File.directory rootDirectoryPath (some sortedChildren)
      pure (firstDirectoryLoaded currentDirectory True.intro)
    | changingDirectory root _ currentDirectory _ displayWidth displayHeight displayRows =>
      match currentDirectory with
      | File.directory _ (some _) =>
        pure ps -- already loaded
      | File.directory path none => do
          let children ← File.readChildren path
          let sortedChildren := File.sortFiles children
          let loadedCurrentDirectory := File.directory path (some sortedChildren)
          let newRoot := root.replaceFile path loadedCurrentDirectory
          match newRoot with
            | File.directory _ newRootChildren =>
              match sortedChildren with
                  | [] => pure (emptyFolderBrowser (File.directory newRoot.path newRootChildren) True.intro (File.directory path (some [])) True.intro displayWidth displayHeight displayRows False)
                  | _ => pure (heightProvided (File.directory newRoot.path newRootChildren) True.intro loadedCurrentDirectory True.intro displayWidth displayHeight displayRows)
            | _ => pure ps -- shouldn't happen - root should always be a directory
    | _ => pure ps


  def withPowerShellStarted (cpp : CodeProxyProcess) (ps : ProgState) : IO ProgState := do
    match ps with
    | emptyFolderBrowser _ _ currentDirectory _ _ _ _ true
    | folderBrowser _ _ currentDirectory _ _ _ _ _ _ _ _ true =>
      cpp.writeCode (Code.startPowerShell currentDirectory.path)
      cpp.writeCode Code.run
      cpp.flush
    | _ => pure ()
    match ps with
    | emptyFolderBrowser root hRootIsDir currentDirectory hCurrentDirIsLoadedEmptyDir displayWidth displayHeight displayRows true => do
      pure (emptyFolderBrowser root hRootIsDir currentDirectory hCurrentDirIsLoadedEmptyDir displayWidth displayHeight displayRows false)
    | folderBrowser root hRootIsDir currentDirectory hCurrentDirIsNonEmptyDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath true =>
      pure (folderBrowser root hRootIsDir currentDirectory hCurrentDirIsNonEmptyDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath false)
    | _ => pure ps -- not starting PowerShell

  def processMovingSelectionForward (ps : ProgState) (_ : isProgStateFolderBrowser ps) (positionsToMove : Nat) : ProgState :=
     match ps with
     | folderBrowser root hRootIsDir currentDirectory hCurrDirIsDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath runPowerShell =>
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
         folderBrowser root hRootIsDir currentDirectory hcd displayWidth displayHeight displayRows displayColumns displayColumnWidth newSelectedFilePath newFileOnTopPath runPowerShell

  def processMovingSelectionBackward (ps : ProgState) (_ : isProgStateFolderBrowser ps) (positionsToMove : Nat) : ProgState :=
    match ps with
    | folderBrowser root hRootIsDir currentDirectory hCurrentDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath runPowerShell =>
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
        folderBrowser root hRootIsDir currentDirectory hcd displayWidth displayHeight displayRows displayColumns displayColumnWidth newSelectedFilePath newFileOnTopPath runPowerShell

  def process (ps : ProgState) (input : String) : ProgState :=
    match ps with
    | start _ => ps
    | firstDirectoryLoaded root hRootIsDir =>
      if input.startsWith "DISPLAY_WIDTH:" then
        widthProvided root hRootIsDir (input.drop "DISPLAY_WIDTH:".length).toNat!
      else ps
    | widthProvided root hRootIsDir displayWidth =>
      if input.startsWith "DISPLAY_HEIGHT:" then
        let height := (input.drop "DISPLAY_HEIGHT:".length).toNat!
        heightProvided root hRootIsDir root hRootIsDir displayWidth height ((height - DisplayConstants.displayTopVerticalMargin - DisplayConstants.displayHeaderFontSize - DisplayConstants.displayHeaderMargin) / DisplayConstants.displayFileFontSize)
      else ps
    | heightProvided root hRootIsDir currentDirectory hCurrentDirIsDir displayWidth displayHeight displayRows =>
      let hcd := hCurrentDirIsDir
      match currentDirectory with
      | File.directory _ optionChildren =>
        match optionChildren with
          | some children => match children with
            | [] => emptyFolderBrowser root hRootIsDir (File.directory currentDirectory.path (some [])) True.intro displayWidth displayHeight displayRows false
            | f :: remainingFiles => if input.startsWith "STR_WIDTH:" then
                let columnWidth := (input.drop "STR_WIDTH:".length).toNat!
                let displayColumnWidth := (columnWidth + DisplayConstants.displayColumnMargin)
                let displayColumns := match displayWidth, (columnWidth + DisplayConstants.displayColumnMargin) with
                  | w, cw => (w / cw) -- todo - remove last extra column margin
                let selectedFilePath := f.path
                let fileOnTopPath := f.path
                folderBrowser root hRootIsDir (File.directory currentDirectory.path (some (f :: remainingFiles))) True.intro displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath false
              else
                ps -- only process STR_WIDTH at this stage
          | none => changingDirectory root hRootIsDir currentDirectory hcd displayWidth displayHeight displayRows
      | _ => ps -- shouldn't happen - currentDirectory should always be a directory
    | emptyFolderBrowser root hRootIsDir currentDirectory hCurrDirIsLoadedEmptyDir displayWidth displayHeight displayRows _ =>
      match input with
      | "KEY_DOWN:KeyBackspace" =>
          match currentDirectory with
          | File.directory _ _ =>
            let parentPath := currentDirectory.parentPath
            let parentDirectory := root.findDirectory parentPath
            match parentDirectory, displayRows with
            | some (File.directory _ (some children)), displayRows =>
              let newCurrentDirectory := File.directory parentPath (some children)
              heightProvided root hRootIsDir newCurrentDirectory True.intro displayWidth displayHeight displayRows
            | _, _ => ps -- shouldn't happen parent directory should always be a directory
      | "KEY_DOWN:KeyP" => emptyFolderBrowser root hRootIsDir currentDirectory hCurrDirIsLoadedEmptyDir displayWidth displayHeight displayRows true
      | "KEY_DOWN:KeyH" => help ps
      | "KEY_DOWN:KeyQ" => exit
      | "DONE." => exit
      | _ => ps -- only process KEY_DOWN:KeyBackspace, KEY_DOWN:KeyQ and DONE
    | folderBrowser root hRootIsDir currentDirectory hCurrentDirIsNonEmptyDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath runPowerShell =>
      let folderBrowserPs := folderBrowser root hRootIsDir currentDirectory hCurrentDirIsNonEmptyDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath runPowerShell
      match input with
      | "KEY_DOWN:KeyDown" => processMovingSelectionForward folderBrowserPs True.intro 1
      | "KEY_DOWN:KeyRight" => processMovingSelectionForward folderBrowserPs True.intro displayRows
      | "KEY_DOWN:KeyPageDown" => processMovingSelectionForward folderBrowserPs True.intro (displayRows * displayColumns)
      | "KEY_DOWN:KeyEnd" => match currentDirectory with
        | File.directory _ (some children) => processMovingSelectionForward folderBrowserPs True.intro children.length
        | _ => ps
      | "KEY_DOWN:KeyUp" => processMovingSelectionBackward folderBrowserPs True.intro 1
      | "KEY_DOWN:KeyLeft" => processMovingSelectionBackward folderBrowserPs True.intro displayRows
      | "KEY_DOWN:KeyPageUp" => processMovingSelectionBackward folderBrowserPs True.intro (displayRows * displayColumns)
      | "KEY_DOWN:KeyHome" => match currentDirectory with
        | File.directory _ (some children) => processMovingSelectionBackward folderBrowserPs True.intro children.length
        | _ => ps
      | "KEY_DOWN:KeyEnter" =>
          match currentDirectory with
          | File.directory _ (some children) =>
            match children.find? (λ f => f.path == selectedFilePath) with
            | some (File.directory path children) =>
              heightProvided root hRootIsDir (File.directory path children) True.intro displayWidth displayHeight displayRows
            | _ => ps -- selected file is not a directory - do nothing
      | "KEY_DOWN:KeyBackspace" =>
          match currentDirectory with
          | File.directory _ _ =>
            let parentPath := currentDirectory.parentPath
            let parentDirectory := root.findDirectory parentPath
            match parentDirectory, displayRows with
            | some (File.directory _ (some children)), displayRows =>
              let newCurrentDirectory := File.directory parentPath (some children)
              heightProvided root hRootIsDir newCurrentDirectory True.intro displayWidth displayHeight displayRows
            | _, _ => ps -- shouldn't happen parent directory should always be a directory
      | "KEY_DOWN:KeyP" => folderBrowser root hRootIsDir currentDirectory hCurrentDirIsNonEmptyDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath true
      | "KEY_DOWN:KeyH" => help ps
      | "KEY_DOWN:KeyQ" => exit
      | "DONE." => exit
      | _ => ps -- only process KEY_DOWN:KeyDown, KEY_DOWN:KeyRight, KEY_DOWN:KeyPageDown, KEY_DOWN:KeyEnd, KEY_DOWN:KeyUp, KEY_DOWN:KeyLeft, KEY_DOWN:KeyPageUp, KEY_DOWN:KeyHome, KEY_DOWN:KeyBackspace, KEY_DOWN:KeyQ and DONE
    | changingDirectory root hRootIsDir currentDirectory hCurrentDirIsDir displayWidth displayHeight displayRows =>
      let hcd := hCurrentDirIsDir
      match currentDirectory with
      | File.directory _ optionChildren =>
        match optionChildren with
          | some children => match children with
            | [] => emptyFolderBrowser root hRootIsDir (File.directory currentDirectory.path (some [])) True.intro displayWidth displayHeight displayRows false
            | f :: remainingFiles =>
              if input.startsWith "STR_WIDTH:" then
                let columnWidth := (input.drop "STR_WIDTH:".length).toNat!
                let displayColumnWidth := (columnWidth + DisplayConstants.displayColumnMargin)
                let displayColumns := match displayWidth, (columnWidth + DisplayConstants.displayColumnMargin) with
                  | w, cw => (w / cw) -- todo - remove last extra column margin
                let selectedFilePath := f.path
                let fileOnTopPath := f.path
                folderBrowser root hRootIsDir (File.directory currentDirectory.path (some (f :: remainingFiles))) True.intro displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath false
              else
                ps -- only process STR_WIDTH at this stage
          | none => changingDirectory root hRootIsDir currentDirectory hcd displayWidth displayHeight displayRows
    | help nextState =>
      match input with
      | "KEY_DOWN:KeyH"
      | "KEY_DOWN:KeyEnter" => nextState
      | "KEY_DOWN:KeyQ" => exit
      | _ => ps -- only process KEY_DOWN:KeyEnter and KEY_DOWN:KeyQ
    | error nextState _ =>
      match input with
      | "KEY_DOWN:KeyEnter" => nextState
      | "KEY_DOWN:KeyQ" => exit
      | _ => ps -- only process KEY_DOWN:KeyEnter and KEY_DOWN:KeyQ
    | exit => ps -- doesn't process any input

end ProgState
