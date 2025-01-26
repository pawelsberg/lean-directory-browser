import LeanDirectoryBrowser.Domain.ProgStates.EmptyFolderBrowser
import LeanDirectoryBrowser.Domain.ProgStates.FolderBrowser
import LeanDirectoryBrowser.Allegro

namespace ProgState

  def withPowerShellStarted (cpp : CodeProxyProcess) (ps : ProgState) : IO ProgState := do
    match h : ps with
    | emptyFolderBrowser _ _ _ _ _ _ _ true => do
      EmptyFolderBrowser.withPowerShellStarted cpp ps (by simp [h, isProgStateEmptyFolderBrowser])
    | folderBrowser _ _ _ _ _ _ _ _ _ _ _ true =>
      FolderBrowser.withPowerShellStarted cpp ps (by simp [h, isProgStateFolderBrowser])
    | _ => pure ps -- other states do not support starting PowerShell

end ProgState
