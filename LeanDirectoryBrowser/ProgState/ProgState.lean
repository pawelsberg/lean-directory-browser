import LeanDirectoryBrowser.File
import LeanDirectoryBrowser.Allegro
import LeanDirectoryBrowser.ProgState.DisplayConstants
import LeanDirectoryBrowser.List

inductive ProgState
| start (currentDirectoryPath : String) -- loading root
| firstDirectoryLoaded (root : File) (hRootIsDir : File.isDirectory root) -- getting display width
| widthProvided (root : File) (hRootIsDir : File.isDirectory root) (displayWidth : Nat) -- getting display height
| heightProvided (root : File) (hRootIsDir : File.isDirectory root) (currentDirectory : File) (hCurrentDirectoryIsDir : File.isDirectory currentDirectory) (displayWidth : Nat) (displayHeight : Nat) (displayRows : Nat) -- getting display column width
| emptyFolderBrowser (root : File) (hRootIsDir : File.isDirectory root) (currentDirectory : File) (hCurrentDirectoryIsLoadedEmptyDir : File.isLoadedEmptyDirectory currentDirectory) (displayWidth : Nat) (displayHeight : Nat) (displayRows : Nat) -- reacting to keys
| folderBrowser (root : File) (hRootIsDir : File.isDirectory root) (currentDirectory : File) (hCurrentDirectoryIsNonEmptyDirectory : File.isLoadedNonEmptyDirectory currentDirectory) (displayWidth : Nat) (displayHeight : Nat) (displayRows : Nat) (displayColumns : Nat) (displayColumnWidth : Nat) (selectedFilePath : String) (fileOnTopPath : String) -- reacting to keys
| changingDirectory (root : File) (hRootIsDir : File.isDirectory root) (currentDirectory : File) (hCurrentDirectoryIsDir : File.isDirectory currentDirectory) (displayWidth : Nat) (displayHeight : Nat) (displayRows : Nat) -- loading files when changing directory
| error (nextState : ProgState) (errorMessage : String)
| exit -- exit program
deriving Repr, BEq

def isProgStateStart : ProgState → Prop
| ProgState.start _ => True
| _ => False

def isProgStateHeightProvided : ProgState → Prop
| ProgState.heightProvided _ _ _ _ _ _ _ => True
| _ => False

def isProgStateFolderBrowser : ProgState → Prop
| ProgState.folderBrowser _ _ _ _ _ _ _ _ _ _ _ => True
| _ => False

def isProgStateEmptyFolderBrowser : ProgState → Prop
| ProgState.emptyFolderBrowser _ _ _ _ _ _ _ => True
| _ => False

def isProgStateChangingDirectory : ProgState → Prop
| ProgState.changingDirectory _ _ _ _ _ _ _ => True
| _ => False

def isProgStateError : ProgState → Prop
| ProgState.error _ _ => True
| _ => False

def isProgStateExit : ProgState → Prop
| ProgState.exit => True
| _ => False

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

def callCodeProxyToRequestColumnWidth (ps : ProgState) (hPsIsEmpty: isProgStateHeightProvided ps) : List Code :=
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

def callCodeProxyWhileWaitingForColumnWidth (ps : ProgramState) : List Code :=
  [
    Code.clearToColor AllegroColor.black,
    Code.drawStoredFontStr DisplayConstants.displayErrorFontColour DisplayConstants.displayTopHorizontalMargin DisplayConstants.displayTopVerticalMargin DisplayConstants.displayErrorFontStorageName FontAlignFlags.left "String width requested previosly"
  ]

def callCodeProxyToDrawFolder (ps : ProgState) (hPsIsEmptyFolderBrowserOrFolderBrowser: isProgStateEmptyFolderBrowser ps ∨ isProgStateFolderBrowser ps ) : List Code :=
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
  | _ => [] -- shouldn't happen


  def callCodeProxyWhileInError (ps : ProgState) (hPsInError: isProgStateError ps) : List Code :=
  match ps with
  | ProgState.error _ errorMessage =>
    [
      Code.clearToColor AllegroColor.black,
      Code.drawStoredFontStr DisplayConstants.displayErrorFontColour DisplayConstants.displayTopHorizontalMargin DisplayConstants.displayTopVerticalMargin DisplayConstants.displayErrorFontStorageName FontAlignFlags.left errorMessage
    ]
  | _ => [] -- shouldn't happen (hPsInError)

  def callCodeProxyWhenColumnWidthNeeded (ps : ProgState) (currentDirectoryPath previousDirectoryPath : String) : List Code :=
    if currentDirectoryPath != previousDirectoryPath then
      (callCodeProxyToRequestColumnWidth ps sorry)
    else
      (callCodeProxyWhileWaitingForColumnWidth ps)

def generateCodeForProxy (ps prevPs: ProgState) : List Code :=
  match ps with
  | ProgState.start currentDirectoryPath =>
    callCodeProxyWhileWaitingForDisplayWidthAndHeight
  | ProgState.firstDirectoryLoaded root hRootIsDir =>
    callCodeProxyWhileWaitingForDisplayWidthAndHeight
  | ProgState.widthProvided root hRootIsDir displayWidth =>
    callCodeProxyWhileWaitingForDisplayWidthAndHeight
  | ProgState.heightProvided root hRootIsDir currentDirectory hCurrentDirectoryIsDir displayWidth displayHeight displayRows =>
    callCodeProxyToRequestColumnWidth (ProgState.heightProvided root hRootIsDir currentDirectory hCurrentDirectoryIsDir displayWidth displayHeight displayRows) True.intro
  | ProgState.emptyFolderBrowser root hRootIsDir currentDirectory hCurrentDirectoryIsLoadedEmptyDir displayWidth displayHeight displayRows =>
    callCodeProxyToDrawFolder (ProgState.emptyFolderBrowser root hRootIsDir currentDirectory hCurrentDirectoryIsLoadedEmptyDir displayWidth displayHeight displayRows) sorry
  | ProgState.folderBrowser root hRootIsDir currentDirectory hCurrentDirectoryIsNonEmptyDirectory displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath =>
    callCodeProxyToDrawFolder (ProgState.folderBrowser root hRootIsDir currentDirectory hCurrentDirectoryIsNonEmptyDirectory displayWidth displayHeight displayRows displayColumns displayColumnWidth selectedFilePath fileOnTopPath) sorry
  | ProgState.changingDirectory root hRootIsDir currentDirectory hCurrentDirectoryIsDir displayWidth displayHeight displayRows =>
    callCodeProxyWhileWaitingForColumnWidth (ProgState.changingDirectory root hRootIsDir currentDirectory hCurrentDirectoryIsDir displayWidth displayHeight displayRows)
  | ProgState.error nextState errorMessage =>
    callCodeProxyWhileInError (ProgState.error nextState errorMessage) True.intro
  | ProgState.exit =>
    []

  def callCodeProxy (ps prevPs: ProgState) (cpp : CodeProxyProcess) : IO Unit := do
    let codeList : List Code := generateCodeForProxy ps prevPs
    Code.writeCodeList cpp codeList
    Code.writeCode cpp Code.run
    cpp.flush
