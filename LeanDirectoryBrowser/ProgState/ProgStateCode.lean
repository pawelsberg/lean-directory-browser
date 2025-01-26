import LeanDirectoryBrowser.ProgState.ProgState

def initFonts : List Code :=
  [
    Code.storeFont DisplayConstants.displayFileFontFileName DisplayConstants.displayFileFontSize DisplayConstants.displayFileFontStorageName,
    Code.storeFont DisplayConstants.displayErrorFontFileName DisplayConstants.displayErrorFontSize DisplayConstants.displayErrorFontStorageName,
    Code.storeFont DisplayConstants.displayHeaderFontFileName DisplayConstants.displayHeaderFontSize DisplayConstants.displayHeaderFontStorageName,
    Code.run
  ]

def destroyFonts : List Code :=
  [
    Code.destroyStoredFont DisplayConstants.displayFileFontStorageName,
    Code.destroyStoredFont DisplayConstants.displayErrorFontStorageName,
    Code.destroyStoredFont DisplayConstants.displayHeaderFontStorageName,
    Code.run
  ]

def callCodeProxyWhileWaitingForDisplayWidthAndHeight : List Code :=
  [
    Code.clearToColor AllegroColor.black,
    Code.drawStoredFontStr DisplayConstants.displayErrorFontColour DisplayConstants.displayTopHorizontalMargin DisplayConstants.displayTopVerticalMargin DisplayConstants.displayErrorFontStorageName FontAlignFlags.left "Waiting to receive display width and height"
  ]

def callCodeProxyToRequestColumnWidth (ps : ProgState) (_: isProgStateHeightProvided ps) : List Code :=
  match ps with
  | ProgState.heightProvided _ _ currentDirectory _ _ _ _ =>
    let maxStringWidth := match currentDirectory with
      | File.directory _ (some children) => children.foldl (λ acc f => Nat.max acc (String.length (File.filename f))) 0
      | _ => DisplayConstants.displayFileDefaultWidth
    [
      Code.requestStrWidth DisplayConstants.displayFileFontFileName DisplayConstants.displayFileFontSize (String.join (List.replicate maxStringWidth "W")),
      Code.run,
      Code.clearToColor AllegroColor.black,
      Code.drawStoredFontStr DisplayConstants.displayErrorFontColour DisplayConstants.displayTopHorizontalMargin DisplayConstants.displayTopVerticalMargin DisplayConstants.displayErrorFontStorageName FontAlignFlags.left "String width just requested"
    ]

def callCodeProxyWhileWaitingForColumnWidth (ps : ProgState) (_: isProgStateChangingDirectory ps) : List Code :=
  [
    Code.clearToColor AllegroColor.black,
    Code.drawStoredFontStr DisplayConstants.displayErrorFontColour DisplayConstants.displayTopHorizontalMargin DisplayConstants.displayTopVerticalMargin DisplayConstants.displayErrorFontStorageName FontAlignFlags.left "String width requested previosly"
  ]

def callCodeProxyToDrawFolder (ps : ProgState) (_: isProgStateAnyFolderBrowser ps ) : List Code :=
  match ps with
  | ProgState.emptyFolderBrowser _ _ currentDirectory _ _ _ _ =>
    [
      Code.clearToColor AllegroColor.black,
      Code.drawStoredFontStr DisplayConstants.displayHeaderFontColour DisplayConstants.displayTopHorizontalMargin DisplayConstants.displayTopVerticalMargin DisplayConstants.displayHeaderFontStorageName FontAlignFlags.left currentDirectory.path
    ]
  | ProgState.folderBrowser root hRootIsDir currentDirectory _ displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath =>
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
          draw_children_columns childrenFromFileOnTop displayColumns 0 (DisplayConstants.displayTopVerticalMargin + DisplayConstants.displayHeaderFontSize + DisplayConstants.displayHeaderMargin)
        ].join

  def callCodeProxyWhileInError (ps : ProgState) (_: isProgStateError ps) : List Code :=
  match ps with
  | ProgState.error _ errorMessage =>
    [
      Code.clearToColor AllegroColor.black,
      Code.drawStoredFontStr DisplayConstants.displayErrorFontColour DisplayConstants.displayTopHorizontalMargin DisplayConstants.displayTopVerticalMargin DisplayConstants.displayErrorFontStorageName FontAlignFlags.left errorMessage
    ]

def generateCodeForProxy (ps : ProgState) : List Code :=
  match ps with
  | ProgState.start _ =>
    callCodeProxyWhileWaitingForDisplayWidthAndHeight
  | ProgState.firstDirectoryLoaded _ _ =>
    callCodeProxyWhileWaitingForDisplayWidthAndHeight
  | ProgState.widthProvided _ _ _ =>
    callCodeProxyWhileWaitingForDisplayWidthAndHeight
  | ProgState.heightProvided root hRootIsDir currentDirectory hCurrentDirectoryIsDir displayWidth displayHeight displayRows =>
    callCodeProxyToRequestColumnWidth (ProgState.heightProvided root hRootIsDir currentDirectory hCurrentDirectoryIsDir displayWidth displayHeight displayRows) True.intro
  | ProgState.emptyFolderBrowser root hRootIsDir currentDirectory hCurrentDirectoryIsLoadedEmptyDir displayWidth displayHeight displayRows =>
    callCodeProxyToDrawFolder (ProgState.emptyFolderBrowser root hRootIsDir currentDirectory hCurrentDirectoryIsLoadedEmptyDir displayWidth displayHeight displayRows) rfl
  | ProgState.folderBrowser root hRootIsDir currentDirectory hCurrentDirectoryIsNonEmptyDirectory displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath =>
    callCodeProxyToDrawFolder (ProgState.folderBrowser root hRootIsDir currentDirectory hCurrentDirectoryIsNonEmptyDirectory displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath) rfl
  | ProgState.changingDirectory root hRootIsDir currentDirectory hCurrentDirectoryIsDir displayWidth displayHeight displayRows =>
    callCodeProxyWhileWaitingForColumnWidth (ProgState.changingDirectory root hRootIsDir currentDirectory hCurrentDirectoryIsDir displayWidth displayHeight displayRows) True.intro
  | ProgState.error nextState errorMessage =>
    callCodeProxyWhileInError (ProgState.error nextState errorMessage) True.intro
  | ProgState.exit =>
    []
