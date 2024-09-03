import LeanDirectoryBrowser.ProgramState

namespace ProgramState

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

  def callCodeProxyWhileWaitingForDisplayWidthAndHeight (ps : ProgramState) (ppc : Al.CodeProxyProcess) : IO Unit := do
    ppc.clearToColor Al.AllegroColor.black
    ppc.drawStoredFontStr ps.displayErrorFontColour ps.displayTopHorizontalMargin ps.displayTopVerticalMargin ps.displayErrorFontStorageName Al.FontAlignFlags.left "Waiting to receive display width and height"

  def callCodeProxyToRequestColumnWidth (ps : ProgramState) (ppc : Al.CodeProxyProcess) : IO Unit := do
    let maxStringWidth := match ps.currentDirectory with
      | some (File.directory _ (some children)) => children.foldl (λ acc f => Nat.max acc (String.length (File.filename f))) 0
      | _ => ps.displayFileDefaultWidth
    ppc.requestStrWidth "consola.ttf" ps.displayFileFontSize (String.join (List.replicate maxStringWidth "W"))
    ppc.run
    ppc.flush
    ppc.clearToColor Al.AllegroColor.black
    ppc.drawStoredFontStr ps.displayErrorFontColour ps.displayTopHorizontalMargin ps.displayTopVerticalMargin ps.displayErrorFontStorageName Al.FontAlignFlags.left "String width just requested"

  def callCodeProxyWhileWaitingForColumnWidth (ps : ProgramState) (ppc : Al.CodeProxyProcess) : IO Unit := do
    ppc.clearToColor Al.AllegroColor.black
    ppc.drawStoredFontStr ps.displayErrorFontColour ps.displayTopHorizontalMargin ps.displayTopVerticalMargin ps.displayErrorFontStorageName Al.FontAlignFlags.left "String width requested previosly"

  def callCodeProxyToDrawFolder (ps : ProgramState) (ppc : Al.CodeProxyProcess) (displayRows displayColumns displayColumnWidth : Nat) : IO Unit := do
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

  def callCodeProxyWhileUnexpectedState (ps : ProgramState) (ppc : Al.CodeProxyProcess) : IO Unit := do
    ppc.clearToColor Al.AllegroColor.black
    ppc.drawStoredFontStr ps.displayErrorFontColour ps.displayTopHorizontalMargin ps.displayTopVerticalMargin ps.displayErrorFontStorageName Al.FontAlignFlags.left "ERROR: unexpected state"

  def callCodeProxyWhenColumnWidthNeeded (ps : ProgramState) (ppc : Al.CodeProxyProcess) (currentDirectoryPath previousDirectoryPath : String) : IO Unit := do
    if currentDirectoryPath != previousDirectoryPath then
      callCodeProxyToRequestColumnWidth ps ppc
    else
      callCodeProxyWhileWaitingForColumnWidth ps ppc

  def callCodeProxy (ps prevPs: ProgramState) (ppc : Al.CodeProxyProcess) : IO Unit := do
    match ps.displayHeight, ps.displayWidth, ps.displayRows, ps.displayColumns, ps.displayColumnWidth, ps.currentDirectoryPath, ps.fileOnTopPath,
      prevPs.displayHeight, prevPs.currentDirectoryPath with
    | none, none, _, _, _, _, _, _,_ => do
      callCodeProxyWhileWaitingForDisplayWidthAndHeight ps ppc
    | none, _, _, _, _, _, _, _, _ => do
      callCodeProxyWhileWaitingForDisplayWidthAndHeight ps ppc
    | _, none, _, _, _, _, _, _, _ => do
      callCodeProxyWhileWaitingForDisplayWidthAndHeight ps ppc
    | _, _, _, _, none, _, _, none, _ => do
      callCodeProxyToRequestColumnWidth ps ppc
    | _, _, _, _, none, currentDirectoryPath, _, _, previousDirectoryPath => do
      callCodeProxyWhenColumnWidthNeeded ps ppc currentDirectoryPath previousDirectoryPath
    | some _, some _, some displayRows, some displayColumns, some displayColumnWidth, _, _, _, _ => do
      callCodeProxyToDrawFolder ps ppc displayRows displayColumns displayColumnWidth
    | _, _, _, _, _, _, _, _, _  => do -- shouldn't happen
      callCodeProxyWhileUnexpectedState ps ppc
    ppc.run
    ppc.flush



end ProgramState
