import LeanDirectoryBrowser.ProgState.ProgState
import LeanDirectoryBrowser.ProgState.StateMachine
import LeanDirectoryBrowser.Allegro

def printWindowsDirectory (children: List File): IO Unit := do
  IO.println (repr children).pretty

def main : IO Unit := do
  let (cpp : CodeProxyProcess) ← IO.Process.spawn {
    cmd := "Al/CodeProxy.Console.exe",
    args := #[],
    stdin := IO.Process.Stdio.piped,
    stdout := IO.Process.Stdio.piped
    }
  cpp.init
  cpp.flush
  Code.writeCodeList cpp initFonts
  cpp.run
  cpp.flush
  let initState := ProgState.start "c:\\"
  let mut state ← initState.withLoadedChildren sorry
  callCodeProxy initState state cpp
  cpp.run
  cpp.flush
  repeat do
    let alOutput ← cpp.getOutputLine
    IO.println ("IN:" ++ alOutput)
    let newState := ProgState.process state alOutput
    let newStateWithLoadedChildren ← ProgState.withLoadedChildren newState.1 sorry
    match newStateWithLoadedChildren with
    | ProgState.exit => do
      IO.println "Program ending..."
      Code.writeCodeList cpp destroyFonts
      cpp.run
      cpp.flush
      cpp.exit
      cpp.run
      cpp.flush
      break
    | _ => do
      if not (newStateWithLoadedChildren == state) then do
        callCodeProxy newStateWithLoadedChildren state cpp
        IO.println "State changed"
        IO.println (repr newStateWithLoadedChildren).pretty
      else do
        IO.println "No state change"
      IO.sleep (ms := 1);
      state := newStateWithLoadedChildren
      continue

  IO.println "Ended."
  cpp.waitForProcessExit >>= IO.print
