import LeanDirectoryBrowser.Domain.ProgStates.Start
import LeanDirectoryBrowser.Domain.ProgStates.FirstDirectoryLoaded
import LeanDirectoryBrowser.Domain.ProgStates.WidthProvided
import LeanDirectoryBrowser.Domain.ProgStates.HeightProvided
import LeanDirectoryBrowser.Domain.ProgStates.EmptyFolderBrowser
import LeanDirectoryBrowser.Domain.ProgStates.FolderBrowser
import LeanDirectoryBrowser.Domain.ProgStates.ChangingDirectory
import LeanDirectoryBrowser.Domain.ProgStates.Help
import LeanDirectoryBrowser.Domain.ProgStates.Error
import LeanDirectoryBrowser.Domain.ProgStates.Exit

namespace ProgState
def getCodeList (ps : ProgState) : List Code :=
  match h: ps with
  | start _ =>
    Start.getCodeList
  | firstDirectoryLoaded _ _ =>
    FirstDirectoryLoaded.getCodeList
  | widthProvided _ _ _ =>
    WidthProvided.getCodeList
  | heightProvided _ _ _ _ _ _ _ =>
    HeightProvided.getCodeList ps (by simp [h, isProgStateHeightProvided])
  | emptyFolderBrowser _ _ _ _ _ _ _ _ =>
    EmptyFolderBrowser.getCodeList ps (by simp [h, isProgStateEmptyFolderBrowser])
  | folderBrowser _ _ _ _ _ _ _ _ _ _ _ _ =>
    FolderBrowser.getCodeList ps (by simp_all [h, isProgStateFolderBrowser])
  | changingDirectory _ _ _ _ _ _ _ =>
    ChangingDirectory.getCodeList ps (by simp [h, isProgStateChangingDirectory])
  | help _ =>
    Help.getCodeList ps (by simp [h, isProgStateHelp])
  | error _ _ =>
    Error.getCodeList ps (by simp [h, isProgStateError])
  | exit =>
    Exit.getCodeList
end ProgState
