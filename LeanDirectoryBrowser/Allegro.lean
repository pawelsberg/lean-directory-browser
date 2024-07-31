namespace Al
  inductive AllegroColor
  | mk (r: Nat) (g: Nat) (b: Nat) : AllegroColor
  def AllegroColor.black : AllegroColor := AllegroColor.mk 0 0 0
  def AllegroColor.white : AllegroColor := AllegroColor.mk 255 255 255

  instance : ToString (AllegroColor) where
  toString ac :=
    match ac with
    | AllegroColor.mk r g b => "Al.MapRgb(" ++ toString r ++ ", " ++ toString g ++ ", " ++ toString b ++ ")"

  inductive FontAlignFlags
  | noKerning : FontAlignFlags
  | left : FontAlignFlags
  | center : FontAlignFlags
  | right : FontAlignFlags
  | integer : FontAlignFlags
  deriving Repr

  instance : ToString FontAlignFlags where
  toString faf :=
    match faf with
    | FontAlignFlags.noKerning => "NoKerning"
    | FontAlignFlags.left => "Left"
    | FontAlignFlags.center => "Center"
    | FontAlignFlags.right => "Right"
    | FontAlignFlags.integer => "Integer"

  inductive FlipFlags
  | none : FlipFlags
  | horizontal : FlipFlags
  | vertical : FlipFlags

  instance : ToString FlipFlags where
  toString ff :=
    match ff with
    | FlipFlags.none => "None"
    | FlipFlags.horizontal => "Horizontal"
    | FlipFlags.vertical => "Vertical"

  def CodeProxyProcess := IO.Process.Child (IO.Process.StdioConfig.mk
    (stdin := IO.Process.Stdio.piped)
    (stdout := IO.Process.Stdio.piped)
    (stderr := IO.Process.Stdio.inherit)
    )
  namespace CodeProxyProcess
    variable (cpp : CodeProxyProcess) (c : AllegroColor)
    variable (x y x1 y1 x2 y2 x3 y3 r thickness : Nat)

    def init : IO Unit := do
      cpp.stdin.putStrLn "Al.Init(); Al.InitFontAddon(); Al.InitTtfAddon(); Al.InitPrimitivesAddon(); Al.InitImageAddon(); Al.InstallKeyboard(); Al.InstallMouse(); var path = Al.GetStandardPath(StandardPath.ResourcesPath); var dataPath = Al.PathCstr(path, '\\\\'); Al.AppendPathComponent(path, \"data\"); Al.ChangeDirectory(Al.PathCstr(path, '\\\\')); Al.DestroyPath(path); Al.SetNewDisplayFlags(DisplayFlags.FullscreenWindow); Al.CreateDisplay(1024, 768); Console.WriteLine(\"DISPLAY_WIDTH:\" + Al.GetDisplayWidth(Al.GetCurrentDisplay())); Console.WriteLine(\"DISPLAY_HEIGHT:\" + Al.GetDisplayHeight(Al.GetCurrentDisplay())); AllegroTimer? timer = Al.CreateTimer(1.0 / 60.0); AllegroEventQueue? eventQueue = Al.CreateEventQueue(); Al.RegisterEventSource(eventQueue, Al.GetDisplayEventSource(Al.GetCurrentDisplay())); Al.RegisterEventSource(eventQueue, Al.GetKeyboardEventSource()); Al.RegisterEventSource(eventQueue, Al.GetMouseEventSource()); Al.RegisterEventSource(eventQueue, Al.GetTimerEventSource(timer)); Al.StartTimer(timer); while (!R.ExitRequested) { AllegroEvent allegroEvent = new AllegroEvent(); Al.WaitForEvent(eventQueue, ref allegroEvent); if (allegroEvent.Type is EventType.KeyDown) { Console.WriteLine($\"KEY_DOWN:{allegroEvent.Keyboard.KeyCode}\"); } if (allegroEvent.Type is EventType.KeyUp) { Console.WriteLine($\"KEY_UP:{allegroEvent.Keyboard.KeyCode}\"); } if (allegroEvent.Type is EventType.DisplayClose) { R.Exit(); } if (allegroEvent.Type is EventType.MouseAxes) { Console.WriteLine($\"MOUSE_AXES:{allegroEvent.Mouse.X},{allegroEvent.Mouse.Y}\"); } if (allegroEvent.Type is EventType.MouseButtonDown) { Console.WriteLine($\"MOUSE_BUTTON_DOWN:{allegroEvent.Mouse.Button}\"); } if (allegroEvent.Type is EventType.MouseButtonUp) { Console.WriteLine($\"MOUSE_BUTTON_UP:{allegroEvent.Mouse.Button}\"); } if (allegroEvent.Type is EventType.Timer) { string code; lock (R.CodeStringBuilder) { code = R.CodeStringBuilder.ToString(); R.CodeStringBuilder.Clear(); } if (code.Length > 0) { R.RunCode(code); Al.FlipDisplay(); } } } Al.DestroyDisplay(Al.GetCurrentDisplay()); Al.UninstallSystem(); Console.WriteLine(\"DONE.\");"
      cpp.stdin.flush
    def flush : IO Unit := do
      cpp.stdin.flush
    def flipDisplay : IO Unit := do
      cpp.stdin.putStrLn "Al.FlipDisplay();"
    def clearToColor : IO Unit := do
      cpp.stdin.putStrLn $ "Al.ClearToColor(" ++ toString c ++ ");"
    def drawStr (fontFileName : String) (size: Nat) (align : FontAlignFlags) (text : String) : IO Unit := do
      cpp.stdin.putStrLn $ "{ var font = Al.LoadTtfFont(Environment.GetFolderPath(Environment.SpecialFolder.Fonts) +  @\"\\" ++ fontFileName ++ "\", " ++ toString size ++ ", LoadFontFlags.None);  Al.DrawUstr( font, " ++ toString c ++ ", " ++ toString x ++ ", " ++ toString y ++ ", FontAlignFlags." ++ toString align ++ ", Al.UstrNew(\"" ++ text ++ "\"));Al.DestroyFont(font);}"
    def drawFilledCircle : IO Unit := do
      cpp.stdin.putStrLn $ "Al.DrawFilledCircle(" ++ toString x ++ ", " ++ toString y ++ ", " ++ toString r ++ ", " ++ toString c ++ ");"
    def drawCircle : IO Unit := do
      cpp.stdin.putStrLn $ "Al.DrawCircle(" ++ toString x ++ ", " ++ toString y ++ ", " ++ toString r ++ ", " ++ toString c ++ ", " ++ toString thickness ++ ");"
    def drawRectangle : IO Unit := do
      cpp.stdin.putStrLn $ "Al.DrawRectangle(" ++ toString x1 ++ ", " ++ toString y1 ++ ", " ++ toString x2 ++ ", " ++ toString y2 ++ ", " ++ toString c ++ ", " ++ toString thickness ++ ");"
    def drawLine : IO Unit := do
      cpp.stdin.putStrLn $ "Al.DrawLine(" ++ toString x1 ++ ", " ++ toString y1 ++ ", " ++ toString x2 ++ ", " ++ toString y2 ++ ", " ++ toString c ++ ", " ++ toString thickness ++ ");"
    def drawTriangle : IO Unit := do
      cpp.stdin.putStrLn $ "Al.DrawTriangle(" ++ toString x1 ++ ", " ++ toString y1 ++ ", " ++ toString x2 ++ ", " ++ toString y2 ++ ", " ++ toString x3 ++ ", " ++ toString y3 ++ ", " ++ toString c ++ ", " ++ toString thickness ++ ");"
    def drawBitmap (bitmapFileName : String) (flipFlags : FlipFlags) : IO Unit := do
      cpp.stdin.putStrLn $ "{ var bitmap = Al.LoadBitmap(@\"" ++ bitmapFileName ++ "\"); Al.DrawBitmap(bitmap, " ++ toString x ++ ", " ++ toString y ++ ", FlipFlags." ++ toString flipFlags ++ ");Al.DestroyBitmap(bitmap);}"
    def exit : IO Unit := do
      cpp.stdin.putStrLn "R.Exit();"
    def run : IO Unit := do
      cpp.stdin.putStrLn "R.Run();"
    def rest (seconds : Nat) : IO Unit := do
      cpp.stdin.putStrLn $ "Al.Rest(" ++ toString seconds ++ ");"
    def getOutputLine : IO String := do
      cpp.stdout.getLine.map String.trim
    def waitForProcessExit : IO UInt32 := do
      IO.Process.Child.wait cpp
  end CodeProxyProcess
end Al
