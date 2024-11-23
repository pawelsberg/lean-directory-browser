import LeanDirectoryBrowser.ProgState.ProgState
import LeanDirectoryBrowser.ProgState.ProgStateCode
import LeanDirectoryBrowser.ProgState.StateMachine
import LeanDirectoryBrowser.Allegro

def main : IO Unit := do
  let (cpp : CodeProxyProcess) ← IO.Process.spawn {
    cmd := "Al/CodeProxy.Console.exe",
    args := #[],
    stdin := IO.Process.Stdio.piped,
    stdout := IO.Process.Stdio.piped
    }
  cpp.writeCode Code.init
  cpp.flush
  cpp.writeCodeList initFonts
  cpp.writeCode Code.run
  cpp.flush
  let initState : ProgState := ProgState.start "c:\\"
  let mut state ← ProgState.withLoadedChildren initState
  cpp.writeCodeList (generateCodeForProxy state)
  cpp.writeCode Code.run
  cpp.flush
  repeat do
    let alOutput ← cpp.getOutputLine
    IO.println ("IN:" ++ alOutput)
    let newStatePossiblyWihoutLoadedChildren := ProgState.process state alOutput
    let newState ← ProgState.withLoadedChildren newStatePossiblyWihoutLoadedChildren
    match newState with
    | ProgState.exit => do
      IO.println "Program ending..."
      cpp.writeCodeList destroyFonts
      cpp.writeCode Code.run
      cpp.flush
      cpp.writeCode Code.exit
      cpp.writeCode Code.run
      cpp.flush
      break
    | _ => do
      if not (newState == state) then do
        cpp.writeCodeList (generateCodeForProxy newState)
        cpp.writeCode Code.run
        cpp.flush
        IO.println "State changed"
        -- IO.println (repr newState).pretty
      else do
        IO.println "No state change"
      IO.sleep (ms := 1);
      state := newState
      continue

  IO.println "Ended."
  cpp.waitForProcessExit >>= IO.print
