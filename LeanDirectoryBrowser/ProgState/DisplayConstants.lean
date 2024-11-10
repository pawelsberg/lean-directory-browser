import LeanDirectoryBrowser.Allegro

namespace DisplayConstants
  def displayHeaderFontSize : Nat := 80
  def displayHeaderFontFileName  : String := "consola.ttf"
  def displayHeaderFontStorageName : String := "header"
  def displayHeaderFontColour : AllegroColor := AllegroColor.mk 220 220 255
  def displayHeaderMargin : Nat := 10
  def displayErrorFontSize : Nat := 100
  def displayErrorFontFileName : String := "consola.ttf"
  def displayErrorFontStorageName : String := "error"
  def displayErrorFontColour : AllegroColor := AllegroColor.mk 255 50 50
  def displayTopHorizontalMargin : Nat := 10
  def displayTopVerticalMargin : Nat := 10
  def displayColumnMargin : Nat := 10
  def displayFileFontSize : Nat := 60
  def displayFileFontFileName : String := "consola.ttf"
  def displayFileFontStorageName : String := "file"
  def displayFileDefaultWidth : Nat := 50
  def displayFileDeselectedFontColour : AllegroColor := AllegroColor.mk 255 255 255
  def displayFileSelectedFontColour : AllegroColor := AllegroColor.mk 255 255 150
end DisplayConstants
