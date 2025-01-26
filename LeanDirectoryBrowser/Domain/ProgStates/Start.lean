import LeanDirectoryBrowser.Domain.ProgState

namespace Start
  def getCodeList : List Code :=
    [
      Code.init,
      Code.run,
      Code.storeFont DisplayConstants.displayFileFontFileName DisplayConstants.displayFileFontSize DisplayConstants.displayFileFontStorageName,
      Code.storeFont DisplayConstants.displayErrorFontFileName DisplayConstants.displayErrorFontSize DisplayConstants.displayErrorFontStorageName,
      Code.storeFont DisplayConstants.displayHeaderFontFileName DisplayConstants.displayHeaderFontSize DisplayConstants.displayHeaderFontStorageName,
      Code.run,
      Code.clearToColor AllegroColor.black,
      Code.drawStoredFontStr DisplayConstants.displayErrorFontColour DisplayConstants.displayTopHorizontalMargin DisplayConstants.displayTopVerticalMargin DisplayConstants.displayErrorFontStorageName FontAlignFlags.left "Waiting to receive display width and height",
      Code.run
    ]

  def withLoadedChildren (ps : ProgState) (_: isProgStateStart ps) : IO ProgState :=
    match ps with
    | ProgState.start rootDirectoryPath => do
      let children ← File.readChildren rootDirectoryPath
      let sortedChildren := File.sortFiles children
      let currentDirectory := File.directory rootDirectoryPath (some sortedChildren)
      pure (ProgState.firstDirectoryLoaded currentDirectory True.intro)
end Start
