import LeanDirectoryBrowser.Allegro
import LeanDirectoryBrowser.File

structure ProgramState :=
  displayWidth : Option Nat
  displayHeight : Option Nat
  root : File
  currentDirectoryPath : String
  selectedFilePath : String
  fileOnTopPath: String
  exitRequested : Bool
  deriving Repr

namespace ProgramState
  def beq : ProgramState → ProgramState → Bool
  | ProgramState.mk displayWidth1 displayHeight1 root1 current_directory_path1 selected_file_path1 file_on_top_path1 exit_requested1,
    ProgramState.mk displayWidth2 displayHeight2 root2 current_directory_path2 selected_file_path2 file_on_top_path2 exit_requested2=>
    displayWidth1 == displayWidth2
    && displayHeight1 == displayHeight2
    && root1 == root2
    && current_directory_path1 == current_directory_path2
    && selected_file_path1 == selected_file_path2
    && file_on_top_path1 == file_on_top_path2
    && exit_requested1 == exit_requested2

  instance : BEq ProgramState := ⟨ProgramState.beq⟩

  def currentDirectory (ps : ProgramState) : Option File :=
    ps.root.findDirectory ps.currentDirectoryPath

  def processKeyDown (ps : ProgramState) : ProgramState :=
    match ps.currentDirectory with
    | File.directory _ children =>
      let rec find_next_file : List File → String → String
      | [], current_file_path => current_file_path
      | f :: fs, current_file_path =>
        if current_file_path == f.path then
          match fs with
          | [] => f.path
          | f' :: _ => f'.path
        else
          find_next_file fs current_file_path
      -- selected_file_path is the next file in the list
      let new_selected_file_path := find_next_file children ps.selectedFilePath
      { ps with selectedFilePath := new_selected_file_path }
    | _ => ps

  def processKeyUp (ps : ProgramState) : ProgramState :=
    match ps.currentDirectory with
    | File.directory _ children =>
      let rec find_previous_file : List File → String → String
      | [], current_file_path => current_file_path
      | f :: fs, current_file_path =>
        match fs with
        | [] => current_file_path
        | f' :: _ =>
          if current_file_path == f'.path  then
              f.path
            else
              find_previous_file fs current_file_path
      -- selected_file_path is the previous file in the list
      let new_selected_file_path := find_previous_file children ps.selectedFilePath
      { ps with selectedFilePath := new_selected_file_path }
    | _ => ps

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
        }
      | _ => ps
    | _ => ps

  def process (ps : ProgramState) (input : String) : ProgramState :=
    if input.startsWith "DISPLAY_WIDTH:" then
      {ps with displayWidth := some (input.drop 14).toNat! }
    else if input.startsWith "DISPLAY_HEIGHT:" then
      {ps with displayHeight := some (input.drop 15).toNat! }
    else
      match input with
      | "KEY_DOWN:KeyDown" => ps.processKeyDown
      -- todo: implement properly the page down
      | "KEY_DOWN:KeyPageDown" => ps.processKeyDown.processKeyDown.processKeyDown.processKeyDown
      -- todo: implement properly the end
      | "KEY_DOWN:KeyEnd" => ps.processKeyDown.processKeyDown.processKeyDown.processKeyDown.processKeyDown.processKeyDown.processKeyDown.processKeyDown
      | "KEY_DOWN:KeyUp" => ps.processKeyUp
      -- todo: implement properly the page up
      | "KEY_DOWN:KeyPageUp" => ps.processKeyUp.processKeyUp.processKeyUp.processKeyUp
      -- todo: implement properly the home
      | "KEY_DOWN:KeyHome" => ps.processKeyUp.processKeyUp.processKeyUp.processKeyUp.processKeyUp.processKeyUp.processKeyUp.processKeyUp
      | "KEY_DOWN:KeyQ" => ps.processKeyQ
      | "KEY_DOWN:KeyEnter" => ps.processKeyEnter
      | "KEY_DOWN:KeyBackspace" => ps.processKeyBackspace
      | "DONE." => { ps with exitRequested := true }
      | _ => ps

  def draw (ppc : Al.CodeProxyProcess) (ps : ProgramState) : IO Unit := do
    if ps.displayWidth == none || ps.displayHeight == none then
      pure ()
    else
      -- todo: use displayWidth and displayHeight
      ppc.clearToColor Al.AllegroColor.black
      ppc.drawStr (Al.AllegroColor.mk 220 220 255) 10 10 "consola.ttf" 40 Al.FontAlignFlags.left ps.currentDirectoryPath

      let draw_file : File → Nat → IO Unit
      | file, y => do
        let filename := file.filename
        let color := if ps.selectedFilePath == file.path then
                      (Al.AllegroColor.mk 255 255 150)
                    else
                      (@Al.AllegroColor.mk 255 255 255)
        ppc.drawStr color 10 y "consola.ttf" 20 Al.FontAlignFlags.left filename

      let rec draw_children_one_level : List File → Nat → IO Unit
        | [], _ => pure ()
        | f :: fs, y => do
          draw_file f y
          draw_children_one_level fs (y + 20)

      match ps.currentDirectory with
        | File.directory _ children =>
          draw_children_one_level children 50
        | _ => pure ()

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
              (children := (List.range 150).map (λ n => (File.file (path := "/root/subdir1/subsubdir1/sfile" ++ toString n)) )),
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
    (currentDirectoryPath := "/root")
    (selectedFilePath := "/root/subdir1")
    (fileOnTopPath := "/root/subdir1")
    (exitRequested := false)


#eval example_program_state.processKeyDown
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
