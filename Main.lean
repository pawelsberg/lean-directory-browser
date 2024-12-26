import LeanDirectoryBrowser.Domain.ProgState
import LeanDirectoryBrowser.Domain.GetCodeList
import LeanDirectoryBrowser.Domain.Process
import LeanDirectoryBrowser.Domain.WithLoadedChildren
import LeanDirectoryBrowser.Domain.WithPowerShellStarted

import LeanDirectoryBrowser.Allegro

def main : IO Unit := do
  let (cpp : CodeProxyProcess) ← IO.Process.spawn {
    cmd := "Al/CodeProxy.Console.exe",
    args := #[],
    stdin := IO.Process.Stdio.piped,
    stdout := IO.Process.Stdio.piped
    : IO.Process.SpawnArgs
    }
  let initState : ProgState := ProgState.start "c:\\"
  cpp.writeCodeList (getCodeList initState)
  cpp.flush
  let mut state ← ProgState.withLoadedChildren initState
  cpp.writeCodeList (getCodeList state)
  cpp.flush
  repeat do
    let alOutput ← cpp.getOutputLine
    IO.println ("IN:" ++ alOutput)
    let newStateProcessed := ProgState.process state alOutput
    let newStatePossiblyWithStartedPowerShell ← ProgState.withPowerShellStarted cpp newStateProcessed
    let newState ← ProgState.withLoadedChildren newStatePossiblyWithStartedPowerShell
    if not (newState == state) then do
      cpp.writeCodeList (getCodeList newState)
      cpp.flush
      IO.println "State changed"
      -- IO.println (repr newState).pretty
    else do
      IO.println "No state change"

    match newState with
    | ProgState.exit => do
      IO.println "Program ending..."
      break
    | _ => do
      IO.sleep (ms := 1);
      state := newState
      continue

  IO.println "Ended."
  cpp.waitForProcessExit >>= IO.print
