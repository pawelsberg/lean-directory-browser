import LeanDirectoryBrowser.Domain.ProgState

def getExitCodeList : List Code :=
  [
      Code.destroyStoredFont DisplayConstants.displayFileFontStorageName,
      Code.destroyStoredFont DisplayConstants.displayErrorFontStorageName,
      Code.destroyStoredFont DisplayConstants.displayHeaderFontStorageName,
      Code.run,
      Code.exit,
      Code.run
  ]
