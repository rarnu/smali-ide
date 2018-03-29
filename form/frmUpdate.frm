inherited FormUpdate: TFormUpdate
  Left = 1365
  Height = 596
  Top = 242
  Width = 396
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Update'
  ClientHeight = 596
  ClientWidth = 396
  Position = poMainFormCenter
  object gbLastVersion: TGroupBox[0]
    Left = 8
    Height = 296
    Top = 8
    Width = 380
    Align = alTop
    BorderSpacing.Around = 8
    Caption = 'Last Version'
    ClientHeight = 277
    ClientWidth = 376
    TabOrder = 0
    object pnlCurrentVersion: TPanel
      Left = 8
      Height = 36
      Top = 8
      Width = 360
      Align = alTop
      BorderSpacing.Around = 8
      BevelOuter = bvNone
      ClientHeight = 36
      ClientWidth = 360
      TabOrder = 0
      object lblCurrentVersion: TLabel
        Left = 0
        Height = 36
        Top = 0
        Width = 120
        Align = alLeft
        AutoSize = False
        Caption = 'Current Version'
        Layout = tlCenter
        ParentColor = False
      end
      object lblCurrentVersionValue: TLabel
        Left = 120
        Height = 36
        Top = 0
        Width = 240
        Align = alClient
        Caption = '0.0.0'
        Layout = tlCenter
        ParentColor = False
      end
    end
    object pnlLastVersion: TPanel
      Left = 8
      Height = 36
      Top = 52
      Width = 360
      Align = alTop
      BorderSpacing.Around = 8
      BevelOuter = bvNone
      ClientHeight = 36
      ClientWidth = 360
      TabOrder = 1
      object lblLastVersion: TLabel
        Left = 0
        Height = 36
        Top = 0
        Width = 120
        Align = alLeft
        AutoSize = False
        Caption = 'Last Version'
        Layout = tlCenter
        ParentColor = False
      end
      object lblLastVersionValue: TLabel
        Left = 120
        Height = 36
        Top = 0
        Width = 240
        Align = alClient
        Caption = '0.0.0'
        Layout = tlCenter
        ParentColor = False
      end
    end
    object mmDesc: TMemo
      Left = 8
      Height = 117
      Top = 104
      Width = 360
      Align = alClient
      BorderSpacing.Top = 8
      BorderSpacing.Around = 8
      BorderSpacing.InnerBorder = 8
      BorderStyle = bsNone
      ReadOnly = True
      ScrollBars = ssAutoVertical
      TabOrder = 2
      WantTabs = True
    end
    object pnlUpdate: TPanel
      Left = 0
      Height = 48
      Top = 229
      Width = 376
      Align = alBottom
      BevelOuter = bvNone
      ClientHeight = 48
      ClientWidth = 376
      TabOrder = 3
      object btnUpdate: TBitBtn
        Left = 293
        Height = 32
        Top = 8
        Width = 75
        Align = alRight
        BorderSpacing.Around = 8
        Caption = 'Update'
        OnClick = btnUpdateClick
        TabOrder = 0
      end
    end
  end
  object gbHistoryVersion: TGroupBox[1]
    Left = 8
    Height = 276
    Top = 312
    Width = 380
    Align = alClient
    BorderSpacing.Around = 8
    Caption = 'History Version'
    ClientHeight = 257
    ClientWidth = 376
    TabOrder = 1
    object sbxHistory: TScrollBox
      Left = 0
      Height = 257
      Top = 0
      Width = 376
      HorzScrollBar.Page = 1
      VertScrollBar.Page = 1
      Align = alClient
      BorderStyle = bsNone
      TabOrder = 0
    end
  end
end
