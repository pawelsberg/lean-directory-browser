import LeanDirectoryBrowser.Domain.ProgState

namespace ChangingDirectory
  def getCodeList (ps : ProgState) (_: isProgStateChangingDirectory ps) : List Code :=
    [
      Code.clearToColor AllegroColor.black,
      Code.drawStoredFontStr DisplayConstants.displayErrorFontColour DisplayConstants.displayTopHorizontalMargin DisplayConstants.displayTopVerticalMargin DisplayConstants.displayErrorFontStorageName FontAlignFlags.left "String width requested previosly",
      Code.run
    ]

  def process (ps : ProgState) (_ : isProgStateChangingDirectory ps) (input : String) : ProgState :=
    match ps with
      | ProgState.changingDirectory root hRootIsDir currentDirectory hCurrentDirIsDir displayWidth displayHeight displayRows =>
        let hcd := hCurrentDirIsDir
        match currentDirectory with
        | File.directory _ optionChildren =>
          match optionChildren with
            | some children => match children with
              | [] => ProgState.emptyFolderBrowser root hRootIsDir (File.directory currentDirectory.path (some [])) True.intro displayWidth displayHeight displayRows false
              | f :: remainingFiles =>
                if input.startsWith "STR_WIDTH:" then
                  let columnWidth := (input.drop "STR_WIDTH:".length).toNat!
                  let displayColumnWidth := (columnWidth + DisplayConstants.displayColumnMargin)
                  let displayColumns := match displayWidth, (columnWidth + DisplayConstants.displayColumnMargin) with
                    | w, cw => (w / cw) -- todo - remove last extra column margin
                  let selectedFilePath := f.path
                  let fileOnTopPath := f.path
                  ProgState.folderBrowser root hRootIsDir (File.directory currentDirectory.path (some (f :: remainingFiles))) True.intro displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath false
                else
                  ps -- only process STR_WIDTH at this stage
            | none => ProgState.changingDirectory root hRootIsDir currentDirectory hcd displayWidth displayHeight displayRows

  def withLoadedChildren (ps : ProgState) (_: isProgStateChangingDirectory ps) : IO ProgState :=
    match ps with
    | ProgState.changingDirectory root _ currentDirectory _ displayWidth displayHeight displayRows =>
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
                  | [] => pure (ProgState.emptyFolderBrowser (File.directory newRoot.path newRootChildren) True.intro (File.directory path (some [])) True.intro displayWidth displayHeight displayRows False)
                  | _ => pure (ProgState.heightProvided (File.directory newRoot.path newRootChildren) True.intro loadedCurrentDirectory True.intro displayWidth displayHeight displayRows)
            | _ => pure ps -- shouldn't happen - root should always be a directory
end ChangingDirectory
