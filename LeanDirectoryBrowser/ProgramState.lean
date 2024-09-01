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

  def displayHeaderFontSize (_ : ProgramState) : Nat := 80
  def displayHeaderFontFileName (_ : ProgramState) : String := "consola.ttf"
  def displayHeaderFontStorageName (_ : ProgramState) : String := "header"
  def displayHeaderFontColour (_ : ProgramState) : Al.AllegroColor := Al.AllegroColor.mk 220 220 255
  def displayHeaderMargin (_ : ProgramState) : Nat := 10
  def displayErrorFontSize (_ : ProgramState) : Nat := 100
  def displayErrorFontFileName (_ : ProgramState) : String := "consola.ttf"
  def displayErrorFontStorageName (_ : ProgramState) : String := "error"
  def displayErrorFontColour (_ : ProgramState) : Al.AllegroColor := Al.AllegroColor.mk 255 50 50
  def displayTopHorizontalMargin (_ : ProgramState) : Nat := 10
  def displayTopVerticalMargin (_ : ProgramState) : Nat := 10
  def displayColumnMargin (_ : ProgramState) : Nat := 10
  def displayFileFontSize (_ : ProgramState) : Nat := 60
  def displayFileFontFileName (_ : ProgramState) : String := "consola.ttf"
  def displayFileFontStorageName (_ : ProgramState) : String := "file"
  def displayFileDefaultWidth (_ : ProgramState) : Nat := 50
  def displayFileDeselectedFontColour (_ : ProgramState) : Al.AllegroColor := Al.AllegroColor.mk 255 255 255
  def displayFileSelectedFontColour (_ : ProgramState) : Al.AllegroColor := Al.AllegroColor.mk 255 255 150

  def initFonts (ps : ProgramState) (ppc : Al.CodeProxyProcess) : IO Unit := do
    ppc.storeFont ps.displayFileFontFileName ps.displayFileFontSize ps.displayFileFontStorageName
    ppc.storeFont ps.displayErrorFontFileName ps.displayErrorFontSize ps.displayErrorFontStorageName
    ppc.storeFont ps.displayHeaderFontFileName ps.displayHeaderFontSize ps.displayHeaderFontStorageName
    ppc.run
    ppc.flush

  def destroyFonts (ps : ProgramState) (ppc : Al.CodeProxyProcess) : IO Unit := do
    ppc.destroyStoredFont ps.displayFileFontStorageName
    ppc.destroyStoredFont ps.displayErrorFontStorageName
    ppc.destroyStoredFont ps.displayHeaderFontStorageName
    ppc.run
    ppc.flush

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

  def callCodeProxy (ps prevPs: ProgramState) (ppc : Al.CodeProxyProcess) : IO Unit := do
    match ps.displayHeight, ps.displayWidth, ps.displayRows, ps.displayColumns,
      ps.displayColumnWidth, ps.currentDirectoryPath, ps.fileOnTopPath, prevPs.displayHeight, prevPs.currentDirectoryPath with
    | none, none, _, _, _, _, _, _,_ => do -- no height or width yet - wait
      ppc.clearToColor Al.AllegroColor.black
      ppc.drawStoredFontStr ps.displayErrorFontColour ps.displayTopHorizontalMargin ps.displayTopVerticalMargin ps.displayErrorFontStorageName Al.FontAlignFlags.left "Waiting for display width and height"
    | none, _, _, _, _, _, _, _, _ => do -- no height or width yet - wait
      ppc.clearToColor Al.AllegroColor.black
      ppc.drawStoredFontStr ps.displayErrorFontColour ps.displayTopHorizontalMargin ps.displayTopVerticalMargin ps.displayErrorFontStorageName Al.FontAlignFlags.left "Waiting for display height"
    | _, none, _, _, _, _, _, _, _ => do -- no height or width yet - wait
      ppc.clearToColor Al.AllegroColor.black
      ppc.drawStoredFontStr ps.displayErrorFontColour ps.displayTopHorizontalMargin ps.displayTopVerticalMargin ps.displayErrorFontStorageName Al.FontAlignFlags.left "Waiting for display width"
    | _, _, _, _, none, _, _, none, _ => do -- dimensions just present - no column width yet calculated - request it
        let maxStringWidth := match ps.currentDirectory with
          | some (File.directory _ (some children)) => children.foldl (λ acc f => Nat.max acc (String.length (File.filename f))) 0
          | _ => ps.displayFileDefaultWidth
        ppc.requestStrWidth "consola.ttf" ps.displayFileFontSize (String.join (List.replicate maxStringWidth "W"))
        ppc.run
        ppc.flush
        ppc.clearToColor Al.AllegroColor.black
        ppc.drawStoredFontStr ps.displayErrorFontColour ps.displayTopHorizontalMargin ps.displayTopVerticalMargin ps.displayErrorFontStorageName Al.FontAlignFlags.left "String width just requested"
    | _, _, _, _, none, currentDirectoryPath, _, _, previousDirectoryPath => do -- dimensions were present previously - so just waiting for the column width
      if currentDirectoryPath != previousDirectoryPath then
        let maxStringWidth := match ps.currentDirectory with
          | some (File.directory _ (some children)) => children.foldl (λ acc f => Nat.max acc (String.length (File.filename f))) 0
          | _ => ps.displayFileDefaultWidth
        ppc.requestStrWidth "consola.ttf" ps.displayFileFontSize (String.join (List.replicate maxStringWidth "W"))
        ppc.run
        ppc.flush
        ppc.clearToColor Al.AllegroColor.black
        ppc.drawStoredFontStr ps.displayErrorFontColour ps.displayTopHorizontalMargin ps.displayTopVerticalMargin ps.displayErrorFontStorageName Al.FontAlignFlags.left "String width just requested after changing folder"
      else
        ppc.clearToColor Al.AllegroColor.black
        ppc.drawStoredFontStr ps.displayErrorFontColour ps.displayTopHorizontalMargin ps.displayTopVerticalMargin ps.displayErrorFontStorageName Al.FontAlignFlags.left "String width requested previosly"
    | some _, some _, some displayRows, some displayColumns, some displayColumnWidth, _, _, _, _ => do
      ppc.clearToColor Al.AllegroColor.black
      ppc.drawStoredFontStr ps.displayHeaderFontColour ps.displayTopHorizontalMargin ps.displayTopVerticalMargin ps.displayHeaderFontStorageName Al.FontAlignFlags.left ps.currentDirectoryPath

      let draw_file : File → Nat → Nat → IO Unit
      | file, x, y => do
        let filename := file.filename
        let color := if ps.selectedFilePath == file.path then
                      ps.displayFileSelectedFontColour
                    else
                      ps.displayFileDeselectedFontColour
        ppc.drawStoredFontStr color x y ps.displayFileFontStorageName Al.FontAlignFlags.left filename

      let rec draw_children_one_level : List File → Nat → Nat → IO Unit
        | [], _, _ => pure ()
        | f :: fs, x, y => do
          draw_file f x y
          draw_children_one_level fs x (y + ps.displayFileFontSize)

      match ps.currentDirectory, ps.fileOnTopPath with
        | File.directory _ (some children), some fileOnTopPath  =>
          let rec draw_children_columns : List File → Nat → Nat → Nat → IO Unit
          | [], _, _, _ => pure ()
          | fs, n, x, y => do
            let (to_draw, rest) := fs.splitAt displayRows
            draw_children_one_level to_draw x y
            if (n > 1) then
              draw_children_columns rest (n - 1) (x + displayColumnWidth) y
          let childrenFromFileOnTop := children.drop (File.indexOfFile children fileOnTopPath)
          draw_children_columns childrenFromFileOnTop displayColumns 0 (ps.displayTopVerticalMargin + ps.displayHeaderFontSize + ps.displayHeaderMargin)
        | _, _ => pure ()
    | _, _, _, _, _, _, _, _, _  => do -- shouldn't happen
      ppc.clearToColor Al.AllegroColor.black
      ppc.drawStoredFontStr ps.displayErrorFontColour ps.displayTopHorizontalMargin ps.displayTopVerticalMargin ps.displayErrorFontStorageName Al.FontAlignFlags.left "ERROR: calculating display parameters"
    ppc.run
    ppc.flush

end ProgramState
