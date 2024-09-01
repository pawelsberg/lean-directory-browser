import LeanDirectoryBrowser.ProgramState

namespace ProgramState
  def processMovingSelectionForward (ps : ProgramState) (positionsToMove : Nat) : ProgramState :=
     match ps.currentDirectory, ps.displayRows, ps.displayColumns, ps.selectedFilePath, ps.fileOnTopPath with
    | File.directory _ (some children), some displayRows, some displayColumns, some selectedFilePath, some fileOnTopPath =>
      let newSelectedFilePath := ((List.range positionsToMove).map toString).foldl (λ currentFilePath _ => File.findNextFilePath children currentFilePath) selectedFilePath
      let moveRightColumns : Nat := ((File.indexOfFile children newSelectedFilePath) - (File.indexOfFile children fileOnTopPath)) / displayRows + 1 - displayColumns
      let newFileOnTopPath := if moveRightColumns > 0
        then
          let newFileOnTopIndex := (File.indexOfFile children fileOnTopPath) + displayRows * moveRightColumns
          match (children.drop newFileOnTopIndex) with
          | [] => ps.fileOnTopPath -- shouldn't happen
          | f :: _ => f.path
        else ps.fileOnTopPath
      { ps with selectedFilePath := newSelectedFilePath,
                fileOnTopPath := newFileOnTopPath}
    | _, _, _, _, _ => ps

  def processKeyDown (ps : ProgramState) : ProgramState :=
    -- selected_file_path is the next file in the list
    processMovingSelectionForward ps 1

  def processKeyRight (ps : ProgramState) : ProgramState :=
      -- selected_file_path is the file in the next column
    match ps.displayRows with
    | some displayRows => processMovingSelectionForward ps displayRows
    | _ => ps

  def processKeyPageDown (ps : ProgramState) : ProgramState :=
      -- selected_file_path is the file on the next page
    match ps.displayRows, ps.displayColumns with
    | some displayRows, some displayColumns => processMovingSelectionForward ps (displayRows * displayColumns)
    | _, _ => ps

  def processKeyEnd (ps : ProgramState) : ProgramState :=
    -- selected_file_path is the last file in the list
    match ps.currentDirectory with
    | File.directory _ (some children) => processMovingSelectionForward ps children.length
    | _ => ps

  def processMovingSelectionBackward (ps : ProgramState) (positionsToMove : Nat) : ProgramState :=
    match ps.currentDirectory, ps.displayRows, ps.selectedFilePath, ps.fileOnTopPath with
    | File.directory _ (some children), some displayRows, some selectedFilePath, some fileOnTopPath =>
      let newSelectedFilePath := ((List.range positionsToMove).map toString).foldl (λ currentFilePath _ => File.findPreviousFilePath children currentFilePath) selectedFilePath
      let moveLeftColumns : Nat := ((File.indexOfFile children fileOnTopPath) - (File.indexOfFile children newSelectedFilePath) + (displayRows - 1) ) / displayRows
      let newFileOnTopPath := if moveLeftColumns > 0
        then
          let newFileOnTopIndex := (File.indexOfFile children fileOnTopPath) - displayRows * moveLeftColumns
          match (children.drop newFileOnTopIndex) with
          | [] => ps.fileOnTopPath -- shouldn't happen
          | f :: _ => f.path
        else ps.fileOnTopPath
      { ps with selectedFilePath := newSelectedFilePath,
                fileOnTopPath := newFileOnTopPath }
    | _, _, _, _ => ps


  def processKeyUp (ps : ProgramState) : ProgramState :=
    -- selected_file_path is the previous file in the list
    processMovingSelectionBackward ps 1

  def processKeyLeft (ps : ProgramState) : ProgramState :=
    -- selected_file_path is the file in the previous column
    match ps.displayRows with
    | some displayRows => processMovingSelectionBackward ps displayRows
    | _ => ps

  def processKeyPageUp (ps : ProgramState) : ProgramState :=
    -- selected_file_path is the file on the previous page
    match ps.displayRows, ps.displayColumns with
    | some displayRows, some displayColumns => processMovingSelectionBackward ps (displayRows * displayColumns)
    | _, _ => ps

  def processKeyHome (ps : ProgramState) : ProgramState :=
    -- selected_file_path is the first file on the list
    match ps.currentDirectory with
    | File.directory _ (some children) => processMovingSelectionBackward ps children.length
    | _ => ps

  def processKeyQ (ps : ProgramState) : ProgramState :=
    { ps with exitRequested := true }

  def processKeyEnter (ps : ProgramState) : ProgramState :=
    match ps.currentDirectory with
    | File.directory _ (some children) =>
      match children.find? (λ f => f.path == ps.selectedFilePath) with
      | some (File.directory path opt_sub_children) => match opt_sub_children with
        | some sub_children =>
        { ps with currentDirectoryPath := path
                  , selectedFilePath := match sub_children with
                                          | [] => none
                                          | f :: _ => some f.path
                  , fileOnTopPath := match sub_children with
                                        | [] => none
                                        | f :: _ => some f.path
                  , displayColumns := none
                  , displayColumnWidth := none
        }
        | none => -- children not loaded yet
        { ps with currentDirectoryPath := path
                  , selectedFilePath := none
                  , fileOnTopPath := none
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
      match parentDirectory, ps.displayRows with
      | some (File.directory _ (some children)), some displayRows =>
        let fileOnTopPath := match children with
          | [] => parentPath
          | (f :: _) => f.path
        { ps with currentDirectoryPath := parentPath
                  , selectedFilePath := some ps.currentDirectoryPath
                  , fileOnTopPath := match children.drop ((File.indexOfFile children ps.currentDirectoryPath) / displayRows * displayRows) with
                                      | [] => fileOnTopPath
                                      | f :: _ => f.path
                  , displayColumns := none
                  , displayColumnWidth := none
        }
      | _, _ => ps
    | _ => ps

  def processDisplayWidth (ps : ProgramState) (input : String) : ProgramState :=
    {ps with
      displayWidth := some (input.drop "DISPLAY_WIDTH:".length).toNat!
    }

  def processDisplayHeight (ps : ProgramState) (input : String) : ProgramState :=
    let height := some (input.drop "DISPLAY_HEIGHT:".length).toNat!
    {ps with
      displayHeight := height
      displayRows := match height with
        | some h => some ((h - ps.displayTopVerticalMargin - ps.displayHeaderFontSize - ps.displayHeaderMargin) / ps.displayFileFontSize)
        | _ => none
    }

  def processStrWidth (ps : ProgramState) (input : String) : ProgramState :=
    let columnWidth := (input.drop "STR_WIDTH:".length).toNat!
    {ps with
      displayColumnWidth := some (columnWidth + ps.displayColumnMargin)
      displayColumns := match ps.displayWidth, (columnWidth + ps.displayColumnMargin) with
        | some w, cw => some (w / cw) -- todo - remove last extra column margin
        | _, _ => none -- shouldn't happen
    }

  def processDone (ps : ProgramState) : ProgramState :=
    { ps with exitRequested := true }

  def process (ps : ProgramState) (input : String) : ProgramState :=
    if input.startsWith "DISPLAY_WIDTH:" then
      ps.processDisplayWidth input
    else if input.startsWith "DISPLAY_HEIGHT:" then
      ps.processDisplayHeight input
    else if input.startsWith "STR_WIDTH:" then
      ps.processStrWidth input
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
      | "DONE." => ps.processDone
      | _ => ps

end ProgramState

def example_program_state : ProgramState :=
  ProgramState.mk
    (File.directory
      (path:= "\\root")
      (children := [
        File.directory
          (path:= "\\root\\subdir1")
          (children := [
            File.directory (path:= "\\root\\subdir1\\subsubdir1")
              (children := (List.range 10).map (λ n => (File.file (path := "\\root\\subdir1\\subsubdir1\\sfile" ++ toString n)) )),
            File.file (path := "\\root\\subdir1\\sfile1"),
            File.file (path := "\\root\\subdir1\\sfile2"),
          ]),
        File.directory
          (path:= "\\root\\subdir2")
          (children := [
            File.file (path := "\\root\\subdir2\\file2_1")
          ]),
        File.file (path := "\\root\\file1"),
        File.file (path := "\\root\\file2"),
      ]))
    (displayWidth := none)
    (displayHeight := none)
    (displayRows := none)
    (displayColumns := none)
    (displayColumnWidth := none)
    (currentDirectoryPath := "\\root")
    (selectedFilePath := "\\root\\subdir1")
    (fileOnTopPath := "\\root\\subdir1")
    (exitRequested := false)

def example_program_state2 : ProgramState :=
  ProgramState.mk
    (root:= File.directory
      (path:= "C:\\Windows")
      (children := [
        File.directory
          (path:= "C:\\Windows\\AppReadiness")
          (some [])
      ]
      )
      )
    (displayWidth := none)
    (displayHeight := none)
    (displayRows := none)
    (displayColumns := none)
    (displayColumnWidth := none)
    (currentDirectoryPath := "C:\\Windows")
    (selectedFilePath := "C:\\Windows\\AppReadiness")
    (fileOnTopPath := "C:\\Windows\\AppReadiness")
    (exitRequested := false)

def example_program_state2_initiated : ProgramState :=
  ((example_program_state2.process "DISPLAY_WIDTH:3456").process "DISPLAY_HEIGHT:2160").process "STR_WIDTH:99"

#eval example_program_state2_initiated
#eval (example_program_state2_initiated.processKeyEnter.process "STR_WIDTH:99")
#eval (example_program_state2_initiated.processKeyEnter.process "STR_WIDTH:99").processKeyBackspace
#eval match (example_program_state2_initiated.processKeyEnter.process "STR_WIDTH:99").currentDirectory with
  | some f => f.parentPath
  | _ => "none"


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
