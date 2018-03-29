inherited FormShortcutAccept: TFormShortcutAccept
  Height = 143
  Width = 317
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Shortcut'
  ClientHeight = 143
  ClientWidth = 317
  KeyPreview = True
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  Position = poMainFormCenter
  object lblPressKey: TLabel[0]
    Left = 0
    Height = 143
    Top = 0
    Width = 317
    Align = alClient
    Alignment = taCenter
    Caption = 'Press a key combo as Shortcut'
    Layout = tlCenter
    ParentColor = False
  end
end
