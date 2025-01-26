import LeanDirectoryBrowser.Domain.ProgState


namespace FirstDirectoryLoaded
  def getCodeList : List Code :=
    [
      Code.clearToColor AllegroColor.black,
      Code.drawStoredFontStr DisplayConstants.displayErrorFontColour DisplayConstants.displayTopHorizontalMargin DisplayConstants.displayTopVerticalMargin DisplayConstants.displayErrorFontStorageName FontAlignFlags.left "Waiting to receive display width and height",
      Code.run
    ]

  def process (ps : ProgState) (_ : isProgStateFirstDirectoryLoaded ps) (input : String) : ProgState :=
    match ps with
    | ProgState.firstDirectoryLoaded root hRootIsDir =>
      if input.startsWith "DISPLAY_WIDTH:" then
        ProgState.widthProvided root hRootIsDir (input.drop "DISPLAY_WIDTH:".length).toNat!
      else ps
end FirstDirectoryLoaded
