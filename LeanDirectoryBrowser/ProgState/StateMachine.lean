import LeanDirectoryBrowser.ProgState.ProgState
import LeanDirectoryBrowser.File
import LeanDirectoryBrowser.ProgState.DisplayConstants

namespace ProgState

  def withLoadedChildren (ps : ProgState) (_: isProgStateLoading ps) : IO ProgState :=
    match ps with
    | start rootDirectoryPath => do
      let children ← File.readChildren rootDirectoryPath
      let currentDirectory := File.directory rootDirectoryPath (some children)
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
                  | [] => pure (emptyFolderBrowser (File.directory newRoot.path newRootChildren) True.intro (File.directory path (some [])) True.intro displayWidth displayHeight displayRows)
                  | _ => pure (heightProvided (File.directory newRoot.path newRootChildren) True.intro loadedCurrentDirectory True.intro displayWidth displayHeight displayRows)
            | _ => pure ps -- shouldn't happen - root should always be a directory
    | _ => pure ps -- This option shouldn't be needed: compiler doesn't need it but it breaks the program if it's not there! Is it a bug in Lean?

  def processMovingSelectionForward (ps : ProgState) (_ : isProgStateFolderBrowser ps) (positionsToMove : Nat) : ProgState :=
     match ps with
     | ProgState.folderBrowser root hRootIsDir currentDirectory hCurrDirIsDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath =>
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
         ProgState.folderBrowser root hRootIsDir currentDirectory hcd displayWidth displayHeight displayRows displayColumns displayColumnWidth newSelectedFilePath newFileOnTopPath

  def processMovingSelectionBackward (ps : ProgState) (_ : isProgStateFolderBrowser ps) (positionsToMove : Nat) : ProgState :=
    match ps with
    | ProgState.folderBrowser root hRootIsDir currentDirectory hCurrentDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath =>
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
        ProgState.folderBrowser root hRootIsDir currentDirectory hcd displayWidth displayHeight displayRows displayColumns displayColumnWidth newSelectedFilePath newFileOnTopPath

  -- Returns a new state and a boolean indicating if the input was processed
  def process (ps : ProgState) (input : String) : ProgState × Bool :=
    match ps with
    | start _ => ⟨ps, false⟩ -- doesn't process - needs IO to load children
    | firstDirectoryLoaded root hRootIsDir =>
      if input.startsWith "DISPLAY_WIDTH:" then
        ⟨ProgState.widthProvided root hRootIsDir (input.drop "DISPLAY_WIDTH:".length).toNat!, true⟩
      else ⟨ps, false⟩
    | widthProvided root hRootIsDir displayWidth =>
      if input.startsWith "DISPLAY_HEIGHT:" then
        let height := (input.drop "DISPLAY_HEIGHT:".length).toNat!
        ⟨ProgState.heightProvided root hRootIsDir root hRootIsDir displayWidth height ((height - DisplayConstants.displayTopVerticalMargin - DisplayConstants.displayHeaderFontSize - DisplayConstants.displayHeaderMargin) / DisplayConstants.displayFileFontSize),true⟩
      else ⟨ps, false⟩
    | heightProvided root hRootIsDir currentDirectory hCurrentDirIsDir displayWidth displayHeight displayRows =>
      let hcd := hCurrentDirIsDir
      match currentDirectory with
      | File.directory _ optionChildren =>
        match optionChildren with
          | some children => match children with
            | [] => ⟨ProgState.emptyFolderBrowser root hRootIsDir (File.directory currentDirectory.path (some [])) True.intro displayWidth displayHeight displayRows, false⟩
            | f :: remainingFiles => if input.startsWith "STR_WIDTH:" then
                let columnWidth := (input.drop "STR_WIDTH:".length).toNat!
                let displayColumnWidth := (columnWidth + DisplayConstants.displayColumnMargin)
                let displayColumns := match displayWidth, (columnWidth + DisplayConstants.displayColumnMargin) with
                  | w, cw => (w / cw) -- todo - remove last extra column margin
                let selectedFilePath := f.path
                let fileOnTopPath := f.path
                ⟨ProgState.folderBrowser root hRootIsDir (File.directory currentDirectory.path (some (f :: remainingFiles))) True.intro displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath, true⟩
              else
                ⟨ps, false⟩ -- only process STR_WIDTH at this stage
          | none => ⟨ProgState.changingDirectory root hRootIsDir currentDirectory hcd displayWidth displayHeight displayRows, false⟩
      | _ => ⟨ps, false⟩ -- shouldn't happen - currentDirectory should always be a directory
    | emptyFolderBrowser root hRootIsDir currentDirectory _ displayWidth displayHeight displayRows =>
      match input with
      | "KEY_DOWN:KeyBackspace" =>
          match currentDirectory with
          | File.directory _ _ =>
            let parentPath := currentDirectory.parentPath
            let parentDirectory := root.findDirectory parentPath
            match parentDirectory, displayRows with
            | some (File.directory _ (some children)), displayRows =>
              let newCurrentDirectory := File.directory parentPath (some children)
              ⟨ProgState.heightProvided root hRootIsDir newCurrentDirectory True.intro displayWidth displayHeight displayRows, true⟩
            | _, _ => ⟨ps, true⟩ -- shouldn't happen parent directory should always be a directory
          | _ => ⟨ps, true⟩ -- shouldn't happen - currentDirectory should always be a directory
      | "KEY_DOWN:KeyQ" => ⟨ProgState.exit, true⟩
      | "DONE." => ⟨ProgState.exit, true⟩
      | _ => ⟨ps, false⟩ -- only process KEY_DOWN:KeyBackspace, KEY_DOWN:KeyQ and DONE
    | folderBrowser root hRootIsDir currentDirectory hCurrentDirIsNonEmptyDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath =>
      let folderBrowserPs := ProgState.folderBrowser root hRootIsDir currentDirectory hCurrentDirIsNonEmptyDir displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath
      match input with
      | "KEY_DOWN:KeyDown" => ⟨processMovingSelectionForward folderBrowserPs True.intro 1, true⟩
      | "KEY_DOWN:KeyRight" => ⟨processMovingSelectionForward folderBrowserPs True.intro displayRows, true⟩
      | "KEY_DOWN:KeyPageDown" => ⟨processMovingSelectionForward folderBrowserPs True.intro (displayRows * displayColumns), true⟩
      | "KEY_DOWN:KeyEnd" => ⟨match currentDirectory with
        | File.directory _ (some children) => processMovingSelectionForward folderBrowserPs True.intro children.length
        | _ => ps, true⟩
      | "KEY_DOWN:KeyUp" => ⟨processMovingSelectionBackward folderBrowserPs True.intro 1, true⟩
      | "KEY_DOWN:KeyLeft" => ⟨processMovingSelectionBackward folderBrowserPs True.intro displayRows, true⟩
      | "KEY_DOWN:KeyPageUp" => ⟨processMovingSelectionBackward folderBrowserPs True.intro (displayRows * displayColumns), true⟩
      | "KEY_DOWN:KeyHome" => ⟨match currentDirectory with
        | File.directory _ (some children) => processMovingSelectionBackward folderBrowserPs True.intro children.length
        | _ => ps, true⟩
      | "KEY_DOWN:KeyEnter" =>
          match currentDirectory with
          | File.directory _ (some children) =>
            match children.find? (λ f => f.path == selectedFilePath) with
            | some (File.directory path children) =>
              ⟨ProgState.changingDirectory root hRootIsDir (File.directory path children) True.intro displayWidth displayHeight displayRows, true⟩
            | _ => ⟨ps, true⟩ -- selected file is not a directory - do nothing
          | _ => ⟨ps, true⟩ -- shouldn't happen - currentDirectory should always be a directory
      | "KEY_DOWN:KeyBackspace" =>
          match currentDirectory with
          | File.directory _ _ =>
            let parentPath := currentDirectory.parentPath
            let parentDirectory := root.findDirectory parentPath
            match parentDirectory, displayRows with
            | some (File.directory _ (some children)), displayRows =>
              let newCurrentDirectory := File.directory parentPath (some children)
              ⟨ProgState.heightProvided root hRootIsDir newCurrentDirectory True.intro displayWidth displayHeight displayRows, true⟩
            | _, _ => ⟨ps, true⟩ -- shouldn't happen parent directory should always be a directory
          | _ => ⟨ps, true⟩ -- shouldn't happen - currentDirectory should always be a directory
      | "KEY_DOWN:KeyQ" => ⟨ProgState.exit, true⟩
      | "DONE." => ⟨ProgState.exit, true⟩
      | _ => ⟨ps, false⟩ -- only process KEY_DOWN:KeyDown, KEY_DOWN:KeyRight, KEY_DOWN:KeyPageDown, KEY_DOWN:KeyEnd, KEY_DOWN:KeyUp, KEY_DOWN:KeyLeft, KEY_DOWN:KeyPageUp, KEY_DOWN:KeyHome, KEY_DOWN:KeyBackspace, KEY_DOWN:KeyQ and DONE
    | changingDirectory root hRootIsDir currentDirectory hCurrentDirIsDir displayWidth displayHeight displayRows =>
      let hcd := hCurrentDirIsDir
      match currentDirectory with
      | File.directory _ optionChildren =>
        match optionChildren with
          | some children => match children with
            | [] => ⟨ProgState.emptyFolderBrowser root hRootIsDir (File.directory currentDirectory.path (some [])) True.intro displayWidth displayHeight displayRows, false⟩
            | f :: remainingFiles => if input.startsWith "STR_WIDTH:" then
                let columnWidth := (input.drop "STR_WIDTH:".length).toNat!
                let displayColumnWidth := (columnWidth + DisplayConstants.displayColumnMargin)
                let displayColumns := match displayWidth, (columnWidth + DisplayConstants.displayColumnMargin) with
                  | w, cw => (w / cw) -- todo - remove last extra column margin
                let selectedFilePath := f.path
                let fileOnTopPath := f.path
                ⟨ProgState.folderBrowser root hRootIsDir (File.directory currentDirectory.path (some (f :: remainingFiles))) True.intro displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath, true⟩
              else
                ⟨ps, false⟩ -- only process STR_WIDTH at this stage
          | none => ⟨ProgState.changingDirectory root hRootIsDir currentDirectory hcd displayWidth displayHeight displayRows, false⟩
      | _ => ⟨ps, false⟩ -- shouldn't happen - currentDirectory should always be a directory
    | error nextState _ =>
      match input with
      | "KEY_DOWN:KeyEnter" => ⟨nextState, true⟩
      | "KEY_DOWN:KeyQ" => ⟨ProgState.exit, true⟩
      | _ => ⟨ps, false⟩ -- only process KEY_DOWN:KeyEnter and KEY_DOWN:KeyQ
    | exit => ⟨ps, false⟩ -- doesn't process any input

end ProgState
