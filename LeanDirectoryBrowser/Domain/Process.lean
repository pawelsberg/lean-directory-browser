import LeanDirectoryBrowser.Domain.ProgStates.Start
import LeanDirectoryBrowser.Domain.ProgStates.FirstDirectoryLoaded
import LeanDirectoryBrowser.Domain.ProgStates.WidthProvided
import LeanDirectoryBrowser.Domain.ProgStates.HeightProvided
import LeanDirectoryBrowser.Domain.ProgStates.EmptyFolderBrowser
import LeanDirectoryBrowser.Domain.ProgStates.FolderBrowser
import LeanDirectoryBrowser.Domain.ProgStates.ChangingDirectory
import LeanDirectoryBrowser.Domain.ProgStates.Help
import LeanDirectoryBrowser.Domain.ProgStates.Error

namespace ProgState

  def process (ps : ProgState) (input : String) : ProgState :=
    match h : ps with
    | start _ => ps -- doesn't process any input
    | firstDirectoryLoaded _ _ =>
      FirstDirectoryLoaded.process ps (by simp [h, isProgStateFirstDirectoryLoaded]) input
    | widthProvided _ _ _ =>
      WidthProvided.process ps (by simp [h, isProgStateWidthProvided]) input
    | heightProvided _ _ _ _ _ _ _ =>
      HeightProvided.process ps (by simp [h, isProgStateHeightProvided]) input
    | emptyFolderBrowser _ _ _ _ _ _ _ _ =>
      EmptyFolderBrowser.process ps (by simp [h, isProgStateEmptyFolderBrowser]) input
    | folderBrowser _ _ _ _ _ _ _ _ _ _ _ _ =>
      FolderBrowser.process ps (by simp [h, isProgStateFolderBrowser]) input
    | changingDirectory _ _ _ _ _ _ _ =>
      ChangingDirectory.process ps (by simp [h, isProgStateChangingDirectory]) input
    | help _ =>
      Help.process ps (by simp [h, isProgStateHelp]) input
    | error _ _ =>
      Error.process ps (by simp [h, isProgStateError]) input
    | exit => ps -- doesn't process any input

end ProgState
