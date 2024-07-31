import LeanDirectoryBrowser.ProgramState
import LeanDirectoryBrowser.Allegro


def main : IO Unit := do
  let (cpp : Al.CodeProxyProcess) ← IO.Process.spawn {
    cmd := "Al/CodeProxy.Console.exe",
    args := #[],
    stdin := IO.Process.Stdio.piped,
    stdout := IO.Process.Stdio.piped
    }
  cpp.init
  cpp.run
  cpp.flush
  let mut state := example_program_state
  state.draw cpp
  cpp.run
  cpp.flush
  repeat do
    let alOutput ← cpp.getOutputLine
    IO.print alOutput
    let newState := ProgramState.process state alOutput
    if not newState.exitRequested then
      if not (newState.beq state) then do
        newState.draw cpp
        cpp.run
        cpp.flush
      else do
        IO.println "No state change"
      IO.sleep (ms:=1)
      state := newState
      continue
    else
      IO.println "Program ending..."
      cpp.exit
      cpp.run
      cpp.flush
      break

  IO.println "Ended."
  cpp.waitForProcessExit >>= IO.print
