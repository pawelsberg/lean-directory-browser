import LeanDirectoryBrowser.Allegro
import LeanDirectoryBrowser.File
import LeanDirectoryBrowser.List

structure ProgramState :=
  displayWidth : Option Nat
  displayHeight : Option Nat
  displayRows: Option Nat
  displayColumns: Option Nat
  displayColumnWidth: Option Nat

  root : File
  currentDirectoryPath : String
  selectedFilePath : String
  fileOnTopPath: String
  exitRequested : Bool
  deriving Repr

namespace ProgramState
  def beq : ProgramState → ProgramState → Bool
  | ProgramState.mk displayWidth1 displayHeight1 displayRows1 displayColumns1 displayColumnWidth1 root1 currentDirectoryPath1 selectedFilePath1 fileOnTopPath1 exitRequested1,
    ProgramState.mk displayWidth2 displayHeight2 displayRows2 displayColumns2 displayColumnWidth2 root2 currentDirectoryPath2 selectedFilePath2 fileOnTopPath2 exitRequested2=>
    displayWidth1 == displayWidth2
    && displayHeight1 == displayHeight2
    && displayRows1 == displayRows2
    && displayColumns1 == displayColumns2
    && displayColumnWidth1 == displayColumnWidth2
    && root1 == root2
    && currentDirectoryPath1 == currentDirectoryPath2
    && selectedFilePath1 == selectedFilePath2
    && fileOnTopPath1 == fileOnTopPath2
    && exitRequested1 == exitRequested2

  instance : BEq ProgramState := ⟨ProgramState.beq⟩

  def currentDirectory (ps : ProgramState) : Option File :=
    ps.root.findDirectory ps.currentDirectoryPath

  def displayHeaderFontSize (_ : ProgramState) : Nat := 80
  def displayHeaderFontColour (_ : ProgramState) : Al.AllegroColor := Al.AllegroColor.mk 220 220 255
  def displayHeaderMargin (_ : ProgramState) : Nat := 10
  def displayErrorFontSize (_ : ProgramState) : Nat := 100
  def displayErrorFontColour (_ : ProgramState) : Al.AllegroColor := Al.AllegroColor.mk 255 50 50
  def displayTopHorizontalMargin (_ : ProgramState) : Nat := 10
  def displayTopVerticalMargin (_ : ProgramState) : Nat := 10
  def displayColumnMargin (_ : ProgramState) : Nat := 10
  def displayFileFontSize (_ : ProgramState) : Nat := 60
  def displayFileDefaultWidth (_ : ProgramState) : Nat := 50
  def displayFileDeselectedFontColour (_ : ProgramState) : Al.AllegroColor := Al.AllegroColor.mk 255 255 255
  def displayFileSelectedFontColour (_ : ProgramState) : Al.AllegroColor := Al.AllegroColor.mk 255 255 150

  def processKeyDown (ps : ProgramState) : ProgramState :=
    match ps.currentDirectory, ps.displayRows, ps.displayColumns with
    | File.directory _ children, some displayRows, some displayColumns =>
      -- selected_file_path is the next file in the list
      let newSelectedFilePath := File.findNextFile children ps.selectedFilePath
      let moveRight := (File.indexOfFile children newSelectedFilePath) > (File.indexOfFile children ps.fileOnTopPath) + displayRows * displayColumns - 1
      let newFileOnTopPath := if moveRight
        then
          let newFileOnTopIndex := (File.indexOfFile children ps.fileOnTopPath) + displayRows
          match (children.drop newFileOnTopIndex) with
          | [] => ps.fileOnTopPath -- shouldn't happen
          | f :: _ => f.path
        else ps.fileOnTopPath
      { ps with selectedFilePath := newSelectedFilePath,
                fileOnTopPath := newFileOnTopPath}
    | _, _, _ => ps

  def processKeyRight (ps : ProgramState) : ProgramState :=
    match ps.currentDirectory, ps.displayRows, ps.displayColumns with
    | File.directory _ children, some displayRows, some displayColumns =>
      -- selected_file_path is the file in the next column
      let newSelectedFilePath := ((List.range displayRows).map toString).foldl (λ currentFilePath _ => File.findNextFile children currentFilePath) ps.selectedFilePath
      let moveRight := (File.indexOfFile children newSelectedFilePath) > (File.indexOfFile children ps.fileOnTopPath) + displayRows * displayColumns - 1
      let newFileOnTopPath := if moveRight
        then
          let newFileOnTopIndex := (File.indexOfFile children ps.fileOnTopPath) + displayRows
          match (children.drop newFileOnTopIndex) with
          | [] => ps.fileOnTopPath -- shouldn't happen
          | f :: _ => f.path
        else ps.fileOnTopPath
      { ps with selectedFilePath := newSelectedFilePath,
                fileOnTopPath := newFileOnTopPath}
    | _, _, _ => ps

  def processKeyPageDown (ps : ProgramState) : ProgramState :=
    match ps.currentDirectory, ps.displayRows, ps.displayColumns with
    | File.directory _ children, some displayRows, some displayColumns =>
      -- selected_file_path is the file on the next page
      let newSelectedFilePath := ((List.range (displayRows * displayColumns)).map toString).foldl (λ currentFilePath _ => File.findNextFile children currentFilePath) ps.selectedFilePath
      let moveRightColumns : Nat := ((File.indexOfFile children newSelectedFilePath) - (File.indexOfFile children ps.fileOnTopPath)) / displayRows - displayColumns + 1
      let newFileOnTopPath := if moveRightColumns > 0
        then
          let newFileOnTopIndex := (File.indexOfFile children ps.fileOnTopPath) + displayRows * moveRightColumns
          match (children.drop newFileOnTopIndex) with
          | [] => ps.fileOnTopPath -- shouldn't happen
          | f :: _ => f.path
        else ps.fileOnTopPath
      { ps with selectedFilePath := newSelectedFilePath,
                fileOnTopPath := newFileOnTopPath }
    | _, _, _ => ps

  def processKeyEnd (ps : ProgramState) : ProgramState :=
    match ps.currentDirectory, ps.displayRows, ps.displayColumns with
    | File.directory _ children, some displayRows, some displayColumns =>
      -- selected_file_path is the last file in the list
      let newSelectedFilePath := ((List.range children.length).map toString).foldl (λ currentFilePath _ => File.findNextFile children currentFilePath) ps.selectedFilePath
      let moveRightColumns : Nat := ((File.indexOfFile children newSelectedFilePath) - (File.indexOfFile children ps.fileOnTopPath)) / displayRows - displayColumns + 1
      let newFileOnTopPath := if moveRightColumns > 0
        then
          let newFileOnTopIndex := (File.indexOfFile children ps.fileOnTopPath) + displayRows * moveRightColumns
          match (children.drop newFileOnTopIndex) with
          | [] => ps.fileOnTopPath -- shouldn't happen
          | f :: _ => f.path
        else ps.fileOnTopPath
      { ps with selectedFilePath := newSelectedFilePath,
                fileOnTopPath := newFileOnTopPath }
    | _, _, _ => ps

  def processKeyUp (ps : ProgramState) : ProgramState :=
    match ps.currentDirectory, ps.displayRows, ps.displayColumns with
    | File.directory _ children, some displayRows, some displayColumns =>
      -- selected_file_path is the previous file in the list
      let newSelectedFilePath := File.findPreviousFile children ps.selectedFilePath
      let moveLeft := (File.indexOfFile children newSelectedFilePath) < (File.indexOfFile children ps.fileOnTopPath)
      let newFileOnTopPath := if moveLeft
        then
          let newFileOnTopIndex := (File.indexOfFile children ps.fileOnTopPath) - displayRows
          match (children.drop newFileOnTopIndex) with
          | [] => ps.fileOnTopPath -- shouldn't happen
          | f :: _ => f.path
        else ps.fileOnTopPath

      { ps with selectedFilePath := newSelectedFilePath,
                fileOnTopPath := newFileOnTopPath }
    | _, _, _ => ps

  def processKeyLeft (ps : ProgramState) : ProgramState :=
    match ps.currentDirectory, ps.displayRows, ps.displayColumns with
    | File.directory _ children, some displayRows, some displayColumns =>
     -- selected_file_path is the file in the previous column
      let newSelectedFilePath := ((List.range displayRows).map toString).foldl (λ currentFilePath _ => File.findPreviousFile children currentFilePath) ps.selectedFilePath
      let moveLeft := (File.indexOfFile children newSelectedFilePath) < (File.indexOfFile children ps.fileOnTopPath)
      let newFileOnTopPath := if moveLeft
        then
          let newFileOnTopIndex := (File.indexOfFile children ps.fileOnTopPath) - displayRows
          match (children.drop newFileOnTopIndex) with
          | [] => ps.fileOnTopPath -- shouldn't happen
          | f :: _ => f.path
        else ps.fileOnTopPath

      { ps with selectedFilePath := newSelectedFilePath,
                fileOnTopPath := newFileOnTopPath }
    | _, _, _ => ps

  def processKeyPageUp (ps : ProgramState) : ProgramState :=
    match ps.currentDirectory, ps.displayRows, ps.displayColumns with
    | File.directory _ children, some displayRows, some displayColumns =>
      -- selected_file_path is the file on the previous page
      let newSelectedFilePath := ((List.range (displayRows * displayColumns)).map toString).foldl (λ currentFilePath _ => File.findPreviousFile children currentFilePath) ps.selectedFilePath
      let moveLeftColumns : Nat := ((File.indexOfFile children ps.fileOnTopPath) - (File.indexOfFile children newSelectedFilePath) - 1) / displayRows + 1
      let newFileOnTopPath := if moveLeftColumns > 0
        then
          let newFileOnTopIndex := (File.indexOfFile children ps.fileOnTopPath) - displayRows * moveLeftColumns
          match (children.drop newFileOnTopIndex) with
          | [] => ps.fileOnTopPath -- shouldn't happen
          | f :: _ => f.path
        else ps.fileOnTopPath
      { ps with selectedFilePath := newSelectedFilePath,
                fileOnTopPath := newFileOnTopPath }
    | _, _, _ => ps

  def processKeyHome (ps : ProgramState) : ProgramState :=
    match ps.currentDirectory, ps.displayRows, ps.displayColumns with
    | File.directory _ children, some displayRows, some _ =>
      -- selected_file_path is the first file on the list
      let newSelectedFilePath := ((List.range children.length).map toString).foldl (λ currentFilePath _ => File.findPreviousFile children currentFilePath) ps.selectedFilePath
      let moveLeftColumns : Nat := ((File.indexOfFile children ps.fileOnTopPath) - (File.indexOfFile children newSelectedFilePath) - 1) / displayRows + 1
      let newFileOnTopPath := if moveLeftColumns > 0
        then
          let newFileOnTopIndex := (File.indexOfFile children ps.fileOnTopPath) - displayRows * moveLeftColumns
          match (children.drop newFileOnTopIndex) with
          | [] => ps.fileOnTopPath -- shouldn't happen
          | f :: _ => f.path
        else ps.fileOnTopPath
      { ps with selectedFilePath := newSelectedFilePath,
                fileOnTopPath := newFileOnTopPath }
    | _, _, _ => ps

  def processKeyQ (ps : ProgramState) : ProgramState :=
    { ps with exitRequested := true }

  def processKeyEnter (ps : ProgramState) : ProgramState :=
    match ps.currentDirectory with
    | File.directory _ children =>
      match children.find? (λ f => f.path == ps.selectedFilePath) with
      | some (File.directory path sub_children) =>
        { ps with currentDirectoryPath := path
                  , selectedFilePath := match sub_children with
                                          | [] => path
                                          | f :: _ => f.path
                  , fileOnTopPath := match sub_children with
                                        | [] => path
                                        | f :: _ => f.path
                  , displayColumns := none
                  , displayColumnWidth := none
        }
      | _ => ps -- shouldn't happen as the selected file should be in the list
    | _ => ps

  def processKeyBackspace (ps : ProgramState) : ProgramState :=
    let currentDirectory := ps.currentDirectory
    match currentDirectory with
    | File.directory path _ =>
      let parentPath := match currentDirectory with
        | some f => f.parentPath
        | none => path
      let parentDirectory := ps.root.findDirectory parentPath
      match parentDirectory with
      | some (File.directory _ children) =>
        let fileOnTopPath := match children with
          | [] => parentPath
          | f :: _ => f.path
        { ps with currentDirectoryPath := parentPath
                  , selectedFilePath := ps.currentDirectoryPath
                  , fileOnTopPath := fileOnTopPath
                  , displayColumns := none
                  , displayColumnWidth := none
        }
      | _ => ps
    | _ => ps

  def process (ps : ProgramState) (input : String) : ProgramState :=
    if input.startsWith "DISPLAY_WIDTH:" then
      {ps with displayWidth := some (input.drop "DISPLAY_WIDTH:".length).toNat! }
    else if input.startsWith "DISPLAY_HEIGHT:" then
      let height := some (input.drop "DISPLAY_HEIGHT:".length).toNat!
      {ps with
        displayHeight := height
        displayRows := match height with
                | some h => some ((h - ps.displayTopVerticalMargin - ps.displayHeaderFontSize - ps.displayHeaderMargin) / ps.displayFileFontSize)
                | _ => none
      }
    else if input.startsWith "STR_WIDTH:" then
      let columnWidth := (input.drop "STR_WIDTH:".length).toNat!
      {ps with displayColumnWidth := some (columnWidth + ps.displayColumnMargin)
              ,displayColumns := match ps.displayWidth, (columnWidth + ps.displayColumnMargin) with
                                  | some w, cw => some (w / cw) -- todo - remove last extra column margin
                                  | _, _ => none -- shouldn't happen

      }
    else
      match input with
      | "KEY_DOWN:KeyDown" => ps.processKeyDown
      | "KEY_DOWN:KeyRight" => ps.processKeyRight
      | "KEY_DOWN:KeyPageDown" => ps.processKeyPageDown
      | "KEY_DOWN:KeyEnd" => ps.processKeyEnd
      | "KEY_DOWN:KeyUp" => ps.processKeyUp
      | "KEY_DOWN:KeyLeft" => ps.processKeyLeft
      | "KEY_DOWN:KeyPageUp" => ps.processKeyPageUp
      | "KEY_DOWN:KeyHome" => ps.processKeyHome
      | "KEY_DOWN:KeyQ" => ps.processKeyQ
      | "KEY_DOWN:KeyEnter" => ps.processKeyEnter
      | "KEY_DOWN:KeyBackspace" => ps.processKeyBackspace
      | "DONE." => { ps with exitRequested := true }
      | _ => ps

  --set_option diagnostics true
  def draw (ps prevPs: ProgramState) (ppc : Al.CodeProxyProcess) : IO Unit := do
    match ps.displayHeight, ps.displayWidth, ps.displayRows, ps.displayColumns,
      ps.displayColumnWidth, ps.currentDirectoryPath, ps.fileOnTopPath, prevPs.displayHeight, prevPs.currentDirectoryPath with
    | none, none, _, _, _, _, _, _,_ => do -- no height or width yet - wait
      ppc.clearToColor Al.AllegroColor.black
      ppc.drawStr ps.displayErrorFontColour ps.displayTopHorizontalMargin ps.displayTopVerticalMargin "consola.ttf" ps.displayErrorFontSize Al.FontAlignFlags.left "Waiting for display width and height"
    | none, _, _, _, _, _, _, _, _ => do -- no height or width yet - wait
      ppc.clearToColor Al.AllegroColor.black
      ppc.drawStr ps.displayErrorFontColour ps.displayTopHorizontalMargin ps.displayTopVerticalMargin "consola.ttf" ps.displayErrorFontSize Al.FontAlignFlags.left "Waiting for display height"
    | _, none, _, _, _, _, _, _, _ => do -- no height or width yet - wait
      ppc.clearToColor Al.AllegroColor.black
      ppc.drawStr ps.displayErrorFontColour ps.displayTopHorizontalMargin ps.displayTopVerticalMargin "consola.ttf" ps.displayErrorFontSize Al.FontAlignFlags.left "Waiting for display width"
    | _, _, _, _, none, _, _, none, _ => do -- dimensions just present - no column width yet calculated - request it
        let maxStringWidth := match ps.currentDirectory with
          | some (File.directory _ children) => children.foldl (λ acc f => Nat.max acc (String.length (File.filename f))) 0
          | _ => ps.displayFileDefaultWidth
        ppc.requestStrWidth "consola.ttf" ps.displayFileFontSize (String.join (List.replicate maxStringWidth "W"))
        ppc.run
        ppc.flush
        ppc.clearToColor Al.AllegroColor.black
        ppc.drawStr ps.displayErrorFontColour ps.displayTopHorizontalMargin ps.displayTopVerticalMargin "consola.ttf" ps.displayErrorFontSize Al.FontAlignFlags.left "String width just requested"
    | _, _, _, _, none, currentDirectoryPath, _, _, previousDirectoryPath => do -- dimensions were present previously - so just waiting for the column width
      if currentDirectoryPath != previousDirectoryPath then
        let maxStringWidth := match ps.currentDirectory with
          | some (File.directory _ children) => children.foldl (λ acc f => Nat.max acc (String.length (File.filename f))) 0
          | _ => ps.displayFileDefaultWidth
        ppc.requestStrWidth "consola.ttf" ps.displayFileFontSize (String.join (List.replicate maxStringWidth "W"))
        ppc.run
        ppc.flush
        ppc.clearToColor Al.AllegroColor.black
        ppc.drawStr ps.displayErrorFontColour ps.displayTopHorizontalMargin ps.displayTopVerticalMargin "consola.ttf" ps.displayErrorFontSize Al.FontAlignFlags.left "String width just requested after changing folder"
      else
        ppc.clearToColor Al.AllegroColor.black
        ppc.drawStr ps.displayErrorFontColour ps.displayTopHorizontalMargin ps.displayTopVerticalMargin "consola.ttf" ps.displayErrorFontSize Al.FontAlignFlags.left "String width requested previosly"
    | some _, some _, some displayRows, some displayColumns, some displayColumnWidth, _, fileOnTopPath, _, _ => do
      ppc.clearToColor Al.AllegroColor.black
      ppc.drawStr ps.displayHeaderFontColour ps.displayTopHorizontalMargin ps.displayTopVerticalMargin "consola.ttf" ps.displayHeaderFontSize Al.FontAlignFlags.left ps.currentDirectoryPath

      let draw_file : File → Nat → Nat → IO Unit
      | file, x, y => do
        let filename := file.filename
        let color := if ps.selectedFilePath == file.path then
                      ps.displayFileSelectedFontColour
                    else
                      ps.displayFileDeselectedFontColour
        ppc.drawStr color x y "consola.ttf" ps.displayFileFontSize Al.FontAlignFlags.left filename

      let rec draw_children_one_level : List File → Nat → Nat → IO Unit
        | [], _, _ => pure ()
        | f :: fs, x, y => do
          draw_file f x y
          draw_children_one_level fs x (y + ps.displayFileFontSize)

      match ps.currentDirectory with
        | File.directory _ children =>
          let rec draw_children_columns : List File → Nat → Nat → Nat → IO Unit
          | [], _, _, _ => pure ()
          | fs, n, x, y => do
            let (to_draw, rest) := fs.splitAt displayRows
            draw_children_one_level to_draw x y
            if (n > 1) then
              draw_children_columns rest (n - 1) (x + displayColumnWidth) y
          let childrenFromFileOnTop := children.drop (File.indexOfFile children fileOnTopPath)
          draw_children_columns childrenFromFileOnTop displayColumns 0 (ps.displayTopVerticalMargin + ps.displayHeaderFontSize + ps.displayHeaderMargin)
        | _ => pure ()
    | _, _, _, _, _, _, _, _, _  => do -- shouldn't happen
      ppc.clearToColor Al.AllegroColor.black
      ppc.drawStr ps.displayErrorFontColour ps.displayTopHorizontalMargin ps.displayTopVerticalMargin "consola.ttf" ps.displayErrorFontSize Al.FontAlignFlags.left "ERROR: calculating display parameters"
    ppc.run
    ppc.flush

end ProgramState

def example_program_state : ProgramState :=
  ProgramState.mk
    (File.directory
      (path:= "/root")
      (children := [
        File.directory
          (path:= "/root/subdir1")
          (children := [
            File.directory (path:= "/root/subdir1/subsubdir1")
              (children := (List.range 6000).map (λ n => (File.file (path := "/root/subdir1/subsubdir1/sfile" ++ toString n)) )),
            File.file (path := "/root/subdir1/sfile1"),
            File.file (path := "/root/subdir1/sfile2"),
          ]),
        File.directory
          (path:= "/root/subdir2")
          (children := [
            File.file (path := "/root/subdir2/file2_1")
          ]),
        File.file (path := "/root/file1"),
        File.file (path := "/root/file2"),
      ]))
    (displayWidth := none)
    (displayHeight := none)
    (displayRows := none)
    (displayColumns := none)
    (displayColumnWidth := none)
    (currentDirectoryPath := "/root")
    (selectedFilePath := "/root/subdir1")
    (fileOnTopPath := "/root/subdir1")
    (exitRequested := false)

#eval ((example_program_state.process "DISPLAY_WIDTH:3456").process "DISPLAY_HEIGHT:2160").process "STR_WIDTH:99"
#eval ((((example_program_state.process "DISPLAY_WIDTH:3456").process "DISPLAY_HEIGHT:2160").process "STR_WIDTH:99").processKeyEnter.processKeyEnter.process "STR_WIDTH:99").processKeyPageDown.processKeyUp.processKeyDown
#eval example_program_state.processKeyEnter
#eval example_program_state.processKeyEnter.processKeyDown
#eval example_program_state.processKeyEnter.processKeyBackspace ==
      example_program_state
#eval example_program_state.processKeyEnter.currentDirectory
#eval match example_program_state.processKeyEnter.currentDirectory with
  | some f => f.parentPath
  | _ => "none"
#eval example_program_state.processKeyDown.processKeyBackspace

#eval [1,2,3,4,5]

#eval (List.range 500)
