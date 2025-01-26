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

  structure Code where
    code: String

  namespace Code
    variable (c : AllegroColor)
    variable (x y x1 y1 x2 y2 x3 y3 r thickness : Nat)

    def run : Code :=
      Code.mk "R.Run();"
    def exit : Code :=
      Code.mk "R.Exit();"
    def rest (seconds : Nat) : Code :=
      Code.mk $ "Al.Rest(" ++ toString seconds ++ ");"
    def init : Code :=
      Code.mk "Al.Init(); Al.InitFontAddon(); Al.InitTtfAddon(); Al.InitPrimitivesAddon(); Al.InitImageAddon(); Al.InstallKeyboard(); Al.InstallMouse(); var path = Al.GetStandardPath(StandardPath.ResourcesPath); var dataPath = Al.PathCstr(path, '\\\\'); Al.AppendPathComponent(path, \"data\"); Al.ChangeDirectory(Al.PathCstr(path, '\\\\')); Al.DestroyPath(path); Al.SetNewDisplayFlags(DisplayFlags.FullscreenWindow); Al.CreateDisplay(1024, 768); Console.WriteLine(\"DISPLAY_WIDTH:\" + Al.GetDisplayWidth(Al.GetCurrentDisplay())); Console.WriteLine(\"DISPLAY_HEIGHT:\" + Al.GetDisplayHeight(Al.GetCurrentDisplay())); AllegroTimer? timer = Al.CreateTimer(1.0 / 60.0); AllegroEventQueue? eventQueue = Al.CreateEventQueue(); Al.RegisterEventSource(eventQueue, Al.GetDisplayEventSource(Al.GetCurrentDisplay())); Al.RegisterEventSource(eventQueue, Al.GetKeyboardEventSource()); Al.RegisterEventSource(eventQueue, Al.GetMouseEventSource()); Al.RegisterEventSource(eventQueue, Al.GetTimerEventSource(timer)); Al.StartTimer(timer); while (!R.ExitRequested) { AllegroEvent allegroEvent = new AllegroEvent(); Al.WaitForEvent(eventQueue, ref allegroEvent); if (allegroEvent.Type is EventType.KeyDown) { Console.WriteLine($\"KEY_DOWN:{allegroEvent.Keyboard.KeyCode}\"); } if (allegroEvent.Type is EventType.KeyUp) { Console.WriteLine($\"KEY_UP:{allegroEvent.Keyboard.KeyCode}\"); } if (allegroEvent.Type is EventType.DisplayClose) { R.Exit(); } if (allegroEvent.Type is EventType.MouseAxes) { Console.WriteLine($\"MOUSE_AXES:{allegroEvent.Mouse.X},{allegroEvent.Mouse.Y}\"); } if (allegroEvent.Type is EventType.MouseButtonDown) { Console.WriteLine($\"MOUSE_BUTTON_DOWN:{allegroEvent.Mouse.Button}\"); } if (allegroEvent.Type is EventType.MouseButtonUp) { Console.WriteLine($\"MOUSE_BUTTON_UP:{allegroEvent.Mouse.Button}\"); } if (allegroEvent.Type is EventType.Timer) { string code; lock (R.CodeStringBuilder) { code = R.CodeStringBuilder.ToString(); R.CodeStringBuilder.Clear(); } if (code.Length > 0) { R.RunCode(code); Al.FlipDisplay(); } } } Al.DestroyDisplay(Al.GetCurrentDisplay()); Al.UninstallSystem(); Console.WriteLine(\"DONE.\");"
    def flipDisplay : Code :=
      Code.mk "Al.FlipDisplay();"
    def escapeString (s : String) : String :=
      s.foldl (fun acc c =>
        if c == '\\' then acc ++ "\\" ++ "\\"
        else if c == '\"' then acc ++ "\\" ++ "\""
        else acc ++ c.toString
      ) ""

    def startPowerShell (workingDirectory : String): Code :=
      Code.mk $ "System.Diagnostics.ProcessStartInfo psi = new System.Diagnostics.ProcessStartInfo { FileName = \"powershell\", UseShellExecute = true, CreateNoWindow = false, WorkingDirectory = @\"" ++ workingDirectory ++"\" }; System.Diagnostics.Process.Start(psi);"
    def clearToColor (c : AllegroColor) : Code :=
      Code.mk $ "Al.ClearToColor(" ++ toString c ++ ");"
    def storeFont (fontFileName : String) (size: Nat) (fontStorageName: String) : Code :=
      Code.mk $ "{ AllegroFont font = Al.LoadTtfFont(Environment.GetFolderPath(Environment.SpecialFolder.Fonts) +  @\"\\" ++ fontFileName ++ "\", " ++ toString size ++ ", LoadFontFlags.None); S.Set<AllegroFont>(\"" ++ fontStorageName ++ "\", font);}"
    def destroyStoredFont (fontStorageName: String) : Code :=
      Code.mk $ "{ AllegroFont font = S.Get<AllegroFont>(\"" ++ fontStorageName ++ "\"); Al.DestroyFont(font); S.Set<AllegroFont>(\"" ++ fontStorageName ++ "\", null);}"
    def drawStoredFontStr (fontStorageName : String) (align : FontAlignFlags) (text : String) : Code :=
      Code.mk $ "{ AllegroFont font = S.Get<AllegroFont>(\"" ++ fontStorageName ++ "\"); Al.DrawUstr( font, " ++ toString c ++ ", " ++ toString x ++ ", " ++ toString y ++ ", FontAlignFlags." ++ toString align ++ ", Al.UstrNew(\"" ++ (escapeString text) ++ "\"));}"
    def drawStr (fontFileName : String) (size: Nat) (align : FontAlignFlags) (text : String) : Code :=
      Code.mk $ "{ var font = Al.LoadTtfFont(Environment.GetFolderPath(Environment.SpecialFolder.Fonts) +  @\"\\" ++ fontFileName ++ "\", " ++ toString size ++ ", LoadFontFlags.None);  Al.DrawUstr( font, " ++ toString c ++ ", " ++ toString x ++ ", " ++ toString y ++ ", FontAlignFlags." ++ toString align ++ ", Al.UstrNew(\"" ++ (escapeString text) ++ "\"));Al.DestroyFont(font);}"
    def requestStrWidth (fontFileName : String) (size: Nat) (text : String) : Code :=
      Code.mk $ "{ var font = Al.LoadTtfFont(Environment.GetFolderPath(Environment.SpecialFolder.Fonts) +  @\"\\" ++ fontFileName ++ "\", " ++ toString size ++ ", LoadFontFlags.None);  var width = Al.GetUstrWidth(font, Al.UstrNew(\"" ++ (escapeString text) ++ "\"));Al.DestroyFont(font); Console.WriteLine(\"STR_WIDTH:\" + width);}"
    def drawFilledCircle : Code :=
      Code.mk $ "Al.DrawFilledCircle(" ++ toString x ++ ", " ++ toString y ++ ", " ++ toString r ++ ", " ++ toString c ++ ");"
    def drawCircle : Code :=
      Code.mk $ "Al.DrawCircle(" ++ toString x ++ ", " ++ toString y ++ ", " ++ toString r ++ ", " ++ toString c ++ ", " ++ toString thickness ++ ");"
    def drawRectangle : Code :=
      Code.mk $ "Al.DrawRectangle(" ++ toString x1 ++ ", " ++ toString y1 ++ ", " ++ toString x2 ++ ", " ++ toString y2 ++ ", " ++ toString c ++ ", " ++ toString thickness ++ ");"
    def drawLine : Code :=
      Code.mk $ "Al.DrawLine(" ++ toString x1 ++ ", " ++ toString y1 ++ ", " ++ toString x2 ++ ", " ++ toString y2 ++ ", " ++ toString c ++ ", " ++ toString thickness ++ ");"
    def drawTriangle : Code :=
      Code.mk $ "Al.DrawTriangle(" ++ toString x1 ++ ", " ++ toString y1 ++ ", " ++ toString x2 ++ ", " ++ toString y2 ++ ", " ++ toString x3 ++ ", " ++ toString y3 ++ ", " ++ toString c ++ ", " ++ toString thickness ++ ");"
    def drawBitmap (bitmapFileName : String) (flipFlags : FlipFlags) : Code :=
      Code.mk $ "{ var bitmap = Al.LoadBitmap(@\"" ++ bitmapFileName ++ "\"); Al.DrawBitmap(bitmap, " ++ toString x ++ ", " ++ toString y ++ ", FlipFlags." ++ toString flipFlags ++ ");Al.DestroyBitmap(bitmap);}"
  end Code

  def CodeProxyProcess := IO.Process.Child (IO.Process.StdioConfig.mk
    (stdin := IO.Process.Stdio.piped)
    (stdout := IO.Process.Stdio.piped)
    (stderr := IO.Process.Stdio.inherit)
    )

  namespace CodeProxyProcess
    variable (cpp : CodeProxyProcess) (c : AllegroColor)
    variable (x y x1 y1 x2 y2 x3 y3 r thickness : Nat)

    def writeCode (code : Code) : IO Unit := do
      cpp.stdin.putStrLn code.code
      IO.print "OUT:"
      IO.println code.code
    def writeCodeList (codes : List Code) : IO Unit := do
      codes.forM (fun code => cpp.stdin.putStrLn code.code)
      IO.println "OUT:CodeList"
      codes.forM (fun code => IO.println code.code)
    def flush : IO Unit := do
      cpp.stdin.flush
    def getOutputLine : IO String := do
      cpp.stdout.getLine.map String.trim
    def waitForProcessExit : IO UInt32 := do
      IO.Process.Child.wait cpp
  end CodeProxyProcess
