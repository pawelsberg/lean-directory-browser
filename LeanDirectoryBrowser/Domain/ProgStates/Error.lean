import LeanDirectoryBrowser.Domain.ProgState

namespace Error
  def getCodeList (ps : ProgState) (_: isProgStateError ps) : List Code :=
    match ps with
      | ProgState.error _ errorMessage =>
        [
          Code.clearToColor AllegroColor.black,
          Code.drawStoredFontStr DisplayConstants.displayErrorFontColour DisplayConstants.displayTopHorizontalMargin DisplayConstants.displayTopVerticalMargin DisplayConstants.displayErrorFontStorageName FontAlignFlags.left errorMessage,
          Code.run
        ]


  def process (ps : ProgState) (_ : isProgStateError ps) (input : String) : ProgState :=
    match ps with
      | ProgState.error nextState _ =>
        match input with
        | "KEY_DOWN:KeyEnter" => nextState
        | "KEY_DOWN:KeyQ" => ProgState.exit
        | _ => ps -- only process KEY_DOWN:KeyEnter and KEY_DOWN:KeyQ
end Error
