import LeanDirectoryBrowser.Domain.ProgState

namespace Help
  def getCodeList (ps : ProgState) (_: isProgStateHelp ps) : List Code :=
    let write : Nat → String → Code := λ (row: Nat) (text: String) => Code.drawStoredFontStr DisplayConstants.displayFileDeselectedFontColour DisplayConstants.displayTopHorizontalMargin (DisplayConstants.displayTopVerticalMargin + DisplayConstants.displayHeaderFontSize + row * DisplayConstants.displayFileFontSize) DisplayConstants.displayFileFontStorageName FontAlignFlags.left text
    [
      Code.clearToColor AllegroColor.black,
      Code.drawStoredFontStr DisplayConstants.displayHeaderFontColour DisplayConstants.displayTopHorizontalMargin DisplayConstants.displayTopVerticalMargin DisplayConstants.displayHeaderFontStorageName FontAlignFlags.left "Help",
      write 1 "Navigation:",
      write 2 "DOWN - Move selection down one file",
      write 3 "RIGHT - Move selection down one column",
      write 4 "PAGE DOWN - Move selection down one page",
      write 5 "END - Move selection to the end of the list",
      write 6 "UP - Move selection up one file",
      write 7 "LEFT - Move selection up one column",
      write 8 "PAGE UP - Move selection up one page",
      write 9 "HOME - Move selection to the beginning of the list",
      write 10 "ENTER - Open selected file or directory",
      write 11 "BACKSPACE - Go up one level",
      write 13 "Miscellaneous:",
      write 14 "P - Run PowerShell in the current directory",
      write 15 "H - Show this help",
      write 16 "Q - Exit program",
      Code.run
    ]

  def process (ps : ProgState) (_ : isProgStateHelp ps) (input : String) : ProgState :=
    match ps with
      | ProgState.help nextState =>
        match input with
        | "KEY_DOWN:KeyH"
        | "KEY_DOWN:KeyEnter" => nextState
        | "KEY_DOWN:KeyQ" => ProgState.exit
        | _ => ps -- only process KEY_DOWN:KeyEnter and KEY_DOWN:KeyQ
end Help
