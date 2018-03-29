inherited FormInputBox: TFormInputBox
  Height = 147
  Width = 430
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 147
  ClientWidth = 430
  Position = poMainFormCenter
  object lblTitle: TLabel[0]
    Left = 16
    Height = 20
    Top = 16
    Width = 398
    Align = alTop
    AutoSize = False
    BorderSpacing.Around = 16
    Layout = tlCenter
    ParentColor = False
  end
  object edtText: TEdit[1]
    Left = 16
    Height = 30
    Top = 52
    Width = 398
    Align = alTop
    AutoSize = False
    BorderSpacing.Around = 16
    TabOrder = 0
  end
  object pnlButton: TPanel[2]
    Left = 8
    Height = 48
    Top = 98
    Width = 414
    Align = alTop
    BorderSpacing.Around = 8
    BevelOuter = bvNone
    ClientHeight = 48
    ClientWidth = 414
    TabOrder = 1
    object btnCancel: TBitBtn
      Left = 248
      Height = 32
      Top = 8
      Width = 75
      Align = alRight
      BorderSpacing.Around = 8
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object btnOK: TBitBtn
      Left = 331
      Height = 32
      Top = 8
      Width = 75
      Align = alRight
      BorderSpacing.Around = 8
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
  end
end
