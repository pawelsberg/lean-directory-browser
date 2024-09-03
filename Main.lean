import LeanDirectoryBrowser.ProgramState
import LeanDirectoryBrowser.ProgramState.Processing
import LeanDirectoryBrowser.ProgramState.CallCodeProxy
import LeanDirectoryBrowser.Allegro

def printWindowsDirectory (children: List File): IO Unit := do
  IO.println (repr children).pretty

def main : IO Unit := do
  let (cpp : Al.CodeProxyProcess) ← IO.Process.spawn {
    cmd := "Al/CodeProxy.Console.exe",
    args := #[],
    stdin := IO.Process.Stdio.piped,
    stdout := IO.Process.Stdio.piped
    }
  cpp.init

  let initState := {example_program_state  with
    root := File.directory "c:\\" none,
    currentDirectoryPath := "c:\\"
  }
  let mut state ← ProgramState.withLoadedChildren initState

  state.initFonts cpp
  cpp.run
  cpp.flush
  repeat do
    let alOutput ← cpp.getOutputLine
    IO.println ("IN:" ++ alOutput)
    let newState := ProgramState.process state alOutput
    let newStateWithLoadedChildren ← ProgramState.withLoadedChildren newState

    if not newStateWithLoadedChildren.exitRequested then
      if not (newStateWithLoadedChildren == state) then do
        newStateWithLoadedChildren.callCodeProxy state cpp
        IO.println "State changed"
        --IO.println (repr newStateWithLoadedChildren).pretty
      else do
        IO.println "No state change"
      IO.sleep (ms:=1)
      state := newStateWithLoadedChildren
      continue
    else
      IO.println "Program ending..."
      newStateWithLoadedChildren.destroyFonts cpp
      cpp.run
      cpp.flush
      cpp.exit
      cpp.run
      cpp.flush
      break

  IO.println "Ended."
  cpp.waitForProcessExit >>= IO.print
