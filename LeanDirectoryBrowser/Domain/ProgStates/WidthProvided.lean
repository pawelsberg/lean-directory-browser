import LeanDirectoryBrowser.Domain.ProgState


def getWidthProvidedCodeList : List Code :=
  [
    Code.clearToColor AllegroColor.black,
    Code.drawStoredFontStr DisplayConstants.displayErrorFontColour DisplayConstants.displayTopHorizontalMargin DisplayConstants.displayTopVerticalMargin DisplayConstants.displayErrorFontStorageName FontAlignFlags.left "Waiting to receive display width and height",
    Code.run
  ]

def processWidthProvided (ps : ProgState) (_ : isProgStateWidthProvided ps) (input : String) : ProgState :=
  match ps with
  | ProgState.widthProvided root hRootIsDir displayWidth =>
    if input.startsWith "DISPLAY_HEIGHT:" then
      let height := (input.drop "DISPLAY_HEIGHT:".length).toNat!
      ProgState.heightProvided root hRootIsDir root hRootIsDir displayWidth height ((height - DisplayConstants.displayTopVerticalMargin - DisplayConstants.displayHeaderFontSize - DisplayConstants.displayHeaderMargin) / DisplayConstants.displayFileFontSize)
    else ps
