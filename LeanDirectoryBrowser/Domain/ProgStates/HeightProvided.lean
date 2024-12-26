import LeanDirectoryBrowser.Domain.ProgState

def getHeightProvidedCodeList (ps : ProgState) (_: isProgStateHeightProvided ps) : List Code :=
  match ps with
  | ProgState.heightProvided _ _ currentDirectory _ _ _ _ =>
    let maxStringWidth := match currentDirectory with
      | File.directory _ (some children) => children.foldl (λ acc f => Nat.max acc (String.length (File.filename f))) 0
      | _ => DisplayConstants.displayFileDefaultWidth
    [
      Code.requestStrWidth DisplayConstants.displayFileFontFileName DisplayConstants.displayFileFontSize (String.join (List.replicate maxStringWidth "W")),
      Code.run,
      Code.clearToColor AllegroColor.black,
      Code.drawStoredFontStr DisplayConstants.displayErrorFontColour DisplayConstants.displayTopHorizontalMargin DisplayConstants.displayTopVerticalMargin DisplayConstants.displayErrorFontStorageName FontAlignFlags.left "String width just requested",
      Code.run
    ]


def processHeightProvided (ps : ProgState) (_ : isProgStateHeightProvided ps) (input : String) : ProgState :=
  match ps with
  | ProgState.heightProvided root hRootIsDir currentDirectory hCurrentDirIsDir displayWidth displayHeight displayRows =>
    let hcd := hCurrentDirIsDir
    match currentDirectory with
    | File.directory _ optionChildren =>
      match optionChildren with
        | some children => match children with
          | [] => ProgState.emptyFolderBrowser root hRootIsDir (File.directory currentDirectory.path (some [])) True.intro displayWidth displayHeight displayRows false
          | f :: remainingFiles => if input.startsWith "STR_WIDTH:" then
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
    | _ => ps -- shouldn't happen - currentDirectory should always be a directory
