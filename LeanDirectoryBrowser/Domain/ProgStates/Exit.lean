import LeanDirectoryBrowser.Domain.ProgState

namespace Exit
  def getCodeList : List Code :=
    [
        Code.destroyStoredFont DisplayConstants.displayFileFontStorageName,
        Code.destroyStoredFont DisplayConstants.displayErrorFontStorageName,
        Code.destroyStoredFont DisplayConstants.displayHeaderFontStorageName,
        Code.run,
        Code.exit,
        Code.run
    ]
end Exit
