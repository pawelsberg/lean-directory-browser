import LeanDirectoryBrowser.Domain.ProgStates.Start
import LeanDirectoryBrowser.Domain.ProgStates.ChangingDirectory

namespace ProgState

  def withLoadedChildren (ps : ProgState) : IO ProgState :=
    match ps with
    | start rootDirectoryPath => do
      withLoadedChildrenStart (start rootDirectoryPath) True.intro
    | changingDirectory root hRootIsDir currentDirectory hCurrDirIsDir displayWidth displayHeight displayRows =>
      withLoadedChildrenChangingDirectory (changingDirectory root hRootIsDir currentDirectory hCurrDirIsDir displayWidth displayHeight displayRows) True.intro
    | _ => pure ps -- other states do not load children

end ProgState
