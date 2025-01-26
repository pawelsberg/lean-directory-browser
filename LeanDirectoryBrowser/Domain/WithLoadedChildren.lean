import LeanDirectoryBrowser.Domain.ProgStates.Start
import LeanDirectoryBrowser.Domain.ProgStates.ChangingDirectory

namespace ProgState

  def withLoadedChildren (ps : ProgState) : IO ProgState :=
    match h : ps with
    | start _ => do
      Start.withLoadedChildren ps (by simp [h, isProgStateStart])
    | changingDirectory _ _ _ _ _ _ _ => do
      ChangingDirectory.withLoadedChildren ps (by simp [h, isProgStateChangingDirectory])
    | _ => pure ps -- other states do not load children

end ProgState
