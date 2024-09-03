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
  selectedFilePath : Option String -- none if empty directory
  fileOnTopPath: Option String -- none if empty directory
  exitRequested : Bool
  deriving Repr, BEq

namespace ProgramState

  def currentDirectory (ps : ProgramState) : Option File :=
    ps.root.findDirectory ps.currentDirectoryPath

  def withLoadedChildren (ps : ProgramState) : IO ProgramState := do
    match ps.currentDirectory with
    | File.directory _ (some _) =>
      pure ps
    | File.directory path none =>
      let children ← File.readChildren path
      let sortedChildren := File.sortFiles children
      pure { ps with
        root := ps.root.replaceFile path (File.directory path (some sortedChildren))
        selectedFilePath := match sortedChildren with
          | [] => none
          | f :: _ => some f.path
        fileOnTopPath := match sortedChildren with
          | [] => none
          | f :: _ => some f.path
      }
    | _ => pure ps

end ProgramState
