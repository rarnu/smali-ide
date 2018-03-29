inherited FormSettings: TFormSettings
  Left = 851
  Height = 600
  Top = 208
  Width = 800
  Caption = 'Settings'
  ClientHeight = 600
  ClientWidth = 800
  Constraints.MinHeight = 600
  Constraints.MinWidth = 800
  Position = poMainFormCenter
  object pgSettings: TPageControl[0]
    Left = 4
    Height = 592
    Top = 4
    Width = 792
    ActivePage = tsEnvironment
    Align = alClient
    BorderSpacing.Around = 4
    TabIndex = 0
    TabOrder = 0
    TabPosition = tpLeft
    object tsEnvironment: TTabSheet
      Caption = 'Environment'
      ClientHeight = 588
      ClientWidth = 688
      object gbJava: TGroupBox
        Left = 8
        Height = 103
        Top = 8
        Width = 672
        Align = alTop
        BorderSpacing.Around = 8
        Caption = 'Java'
        ClientHeight = 84
        ClientWidth = 668
        TabOrder = 0
        object pnlJavaDefault: TPanel
          Left = 0
          Height = 40
          Top = 0
          Width = 668
          Align = alTop
          BevelColor = clNone
          BevelOuter = bvNone
          ClientHeight = 40
          ClientWidth = 668
          TabOrder = 0
          object lblDefaultJava: TLabel
            Left = 8
            Height = 40
            Top = 0
            Width = 180
            Align = alLeft
            AutoSize = False
            BorderSpacing.Left = 8
            Caption = 'Default: /usr/bin/java'
            Layout = tlCenter
            ParentColor = False
          end
          object lblJavaStatus: TLabel
            Left = 196
            Height = 24
            Top = 8
            Width = 100
            Align = alLeft
            AutoSize = False
            BorderSpacing.Around = 8
            Caption = '(exist)'
            Layout = tlCenter
            ParentColor = False
            ParentFont = False
          end
        end
        object pnlJavaChoose: TPanel
          Left = 0
          Height = 44
          Top = 40
          Width = 668
          Align = alClient
          BevelOuter = bvNone
          ClientHeight = 44
          ClientWidth = 668
          TabOrder = 1
          object lblchooseJava: TLabel
            Left = 8
            Height = 44
            Top = 0
            Width = 180
            Align = alLeft
            AutoSize = False
            BorderSpacing.Left = 8
            Caption = 'Choose Java Binary Path'
            Layout = tlCenter
            ParentColor = False
          end
          object btnChooseJava: TBitBtn
            Left = 632
            Height = 28
            Top = 8
            Width = 28
            Align = alRight
            BorderSpacing.Around = 8
            Caption = '...'
            OnClick = btnChooseJavaClick
            TabOrder = 0
          end
          object edtJavaPath: TEdit
            Left = 196
            Height = 28
            Top = 8
            Width = 428
            Align = alClient
            BorderSpacing.Around = 8
            TabOrder = 1
          end
        end
      end
      object gbCurl: TGroupBox
        Left = 8
        Height = 103
        Top = 119
        Width = 672
        Align = alTop
        BorderSpacing.Around = 8
        Caption = 'Curl'
        ClientHeight = 84
        ClientWidth = 668
        TabOrder = 1
        object pnlCurlDefault: TPanel
          Left = 0
          Height = 40
          Top = 0
          Width = 668
          Align = alTop
          BevelColor = clNone
          BevelOuter = bvNone
          ClientHeight = 40
          ClientWidth = 668
          TabOrder = 0
          object lblDefaultCurl: TLabel
            Left = 8
            Height = 40
            Top = 0
            Width = 180
            Align = alLeft
            AutoSize = False
            BorderSpacing.Left = 8
            Caption = 'Default: /usr/bin/curl'
            Layout = tlCenter
            ParentColor = False
          end
          object lblCurlStatus: TLabel
            Left = 196
            Height = 24
            Top = 8
            Width = 100
            Align = alLeft
            AutoSize = False
            BorderSpacing.Around = 8
            Caption = '(exist)'
            Layout = tlCenter
            ParentColor = False
            ParentFont = False
          end
        end
        object pnlCurlChoose: TPanel
          Left = 0
          Height = 44
          Top = 40
          Width = 668
          Align = alClient
          BevelOuter = bvNone
          ClientHeight = 44
          ClientWidth = 668
          TabOrder = 1
          object lblChooseCurl: TLabel
            Left = 8
            Height = 44
            Top = 0
            Width = 180
            Align = alLeft
            AutoSize = False
            BorderSpacing.Left = 8
            Caption = 'Choose Curl Binary Path'
            Layout = tlCenter
            ParentColor = False
          end
          object btnChooseCurl: TBitBtn
            Left = 632
            Height = 28
            Top = 8
            Width = 28
            Align = alRight
            BorderSpacing.Around = 8
            Caption = '...'
            OnClick = btnChooseCurlClick
            TabOrder = 0
          end
          object edtCurlPath: TEdit
            Left = 196
            Height = 28
            Top = 8
            Width = 428
            Align = alClient
            BorderSpacing.Around = 8
            TabOrder = 1
          end
        end
      end
      object gbApktool: TGroupBox
        Left = 8
        Height = 103
        Top = 230
        Width = 672
        Align = alTop
        BorderSpacing.Around = 8
        Caption = 'Apktool'
        ClientHeight = 84
        ClientWidth = 668
        TabOrder = 2
        object pnlApktoolDefault: TPanel
          Left = 0
          Height = 40
          Top = 0
          Width = 668
          Align = alTop
          BevelColor = clNone
          BevelOuter = bvNone
          ClientHeight = 40
          ClientWidth = 668
          TabOrder = 0
          object lblDefaultApktool: TLabel
            Left = 8
            Height = 40
            Top = 0
            Width = 180
            Align = alLeft
            AutoSize = False
            BorderSpacing.Left = 8
            Caption = 'Default: apktool.jar'
            Layout = tlCenter
            ParentColor = False
          end
          object lblApktoolStatus: TLabel
            Left = 196
            Height = 24
            Top = 8
            Width = 100
            Align = alLeft
            AutoSize = False
            BorderSpacing.Around = 8
            Caption = '(exist)'
            Layout = tlCenter
            ParentColor = False
            ParentFont = False
          end
        end
        object pnlApktoolVersion: TPanel
          Left = 0
          Height = 44
          Top = 40
          Width = 668
          Align = alClient
          BevelOuter = bvNone
          ClientHeight = 44
          ClientWidth = 668
          TabOrder = 1
          object lblApktoolVersion: TLabel
            Left = 8
            Height = 44
            Top = 0
            Width = 180
            Align = alLeft
            AutoSize = False
            BorderSpacing.Left = 8
            Caption = 'Version'
            Layout = tlCenter
            ParentColor = False
          end
          object lblApktoolVersionValue: TLabel
            Left = 196
            Height = 28
            Top = 8
            Width = 464
            Align = alClient
            BorderSpacing.Around = 8
            Caption = '0.0.0'
            Layout = tlCenter
            ParentColor = False
          end
        end
      end
      object gbJadx: TGroupBox
        Left = 8
        Height = 103
        Top = 341
        Width = 672
        Align = alTop
        BorderSpacing.Around = 8
        Caption = 'Jadx'
        ClientHeight = 84
        ClientWidth = 668
        TabOrder = 3
        object pnlJadxDefault: TPanel
          Left = 0
          Height = 40
          Top = 0
          Width = 668
          Align = alTop
          BevelColor = clNone
          BevelOuter = bvNone
          ClientHeight = 40
          ClientWidth = 668
          TabOrder = 0
          object lblJadxDefault: TLabel
            Left = 8
            Height = 40
            Top = 0
            Width = 180
            Align = alLeft
            AutoSize = False
            BorderSpacing.Left = 8
            Caption = 'Default: jadx'
            Layout = tlCenter
            ParentColor = False
          end
          object lblJadxStatus: TLabel
            Left = 196
            Height = 24
            Top = 8
            Width = 100
            Align = alLeft
            AutoSize = False
            BorderSpacing.Around = 8
            Caption = '(exist)'
            Layout = tlCenter
            ParentColor = False
            ParentFont = False
          end
        end
        object pnlJadxVersion: TPanel
          Left = 0
          Height = 44
          Top = 40
          Width = 668
          Align = alClient
          BevelOuter = bvNone
          ClientHeight = 44
          ClientWidth = 668
          TabOrder = 1
          object lblJadxlVersion: TLabel
            Left = 8
            Height = 44
            Top = 0
            Width = 180
            Align = alLeft
            AutoSize = False
            BorderSpacing.Left = 8
            Caption = 'Version'
            Layout = tlCenter
            ParentColor = False
          end
          object lblJadxVersionValue: TLabel
            Left = 196
            Height = 28
            Top = 8
            Width = 464
            Align = alClient
            BorderSpacing.Around = 8
            Caption = '0.0.0'
            Layout = tlCenter
            ParentColor = False
          end
        end
      end
      object gbAndroidSDK: TGroupBox
        Left = 8
        Height = 116
        Top = 452
        Width = 672
        Align = alTop
        BorderSpacing.Around = 8
        Caption = 'Android SDK'
        ClientHeight = 97
        ClientWidth = 668
        TabOrder = 4
        object pnlAndroidSDKPath: TPanel
          Left = 0
          Height = 44
          Top = 0
          Width = 668
          Align = alTop
          BevelOuter = bvNone
          ClientHeight = 44
          ClientWidth = 668
          TabOrder = 0
          object lblChooseSDKPath: TLabel
            Left = 8
            Height = 44
            Top = 0
            Width = 180
            Align = alLeft
            AutoSize = False
            BorderSpacing.Left = 8
            Caption = 'Choose SDK Path'
            Layout = tlCenter
            ParentColor = False
          end
          object btnChooseSDKPath: TBitBtn
            Left = 632
            Height = 28
            Top = 8
            Width = 28
            Align = alRight
            BorderSpacing.Around = 8
            Caption = '...'
            OnClick = btnChooseSDKPathClick
            TabOrder = 0
          end
          object edtAndroidSDKPath: TEdit
            Left = 196
            Height = 28
            Top = 8
            Width = 428
            Align = alClient
            BorderSpacing.Around = 8
            TabOrder = 1
          end
        end
        object pnlAndroidVersion: TPanel
          Left = 0
          Height = 44
          Top = 44
          Width = 668
          Align = alTop
          BevelOuter = bvNone
          ClientHeight = 44
          ClientWidth = 668
          TabOrder = 1
          object lblChooseAndroidVersion: TLabel
            Left = 8
            Height = 44
            Top = 0
            Width = 180
            Align = alLeft
            AutoSize = False
            BorderSpacing.Left = 8
            Caption = 'Choose Android Version'
            Layout = tlCenter
            ParentColor = False
          end
          object cbAndroidVersion: TComboBox
            Left = 196
            Height = 28
            Top = 8
            Width = 464
            Align = alClient
            BorderSpacing.Around = 8
            Color = clWhite
            Font.Color = clBlack
            ItemHeight = 0
            Items.Strings = (
              '1111'
              '222'
            )
            OnChange = cbAndroidVersionChange
            ParentFont = False
            TabOrder = 0
          end
        end
      end
    end
    object tsShortcut: TTabSheet
      Caption = 'Shortcut'
      ClientHeight = 588
      ClientWidth = 688
      object sbxShortcut: TScrollBox
        Left = 0
        Height = 588
        Top = 0
        Width = 688
        HorzScrollBar.Page = 290
        VertScrollBar.Page = 588
        Align = alClient
        BorderStyle = bsNone
        ClientHeight = 588
        ClientWidth = 685
        TabOrder = 0
        object gbView: TGroupBox
          Left = 8
          Height = 264
          Top = 8
          Width = 656
          Align = alTop
          BorderSpacing.Around = 8
          Caption = 'View'
          ClientHeight = 245
          ClientWidth = 652
          TabOrder = 0
          object pnlViewClassIndex: TPanel
            Left = 0
            Height = 40
            Top = 0
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 0
            object lblViewClassIndex: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'Switch Class Index View'
              Layout = tlCenter
              ParentColor = False
            end
            object btnViewClassIndex: TBitBtn
              Left = 444
              Height = 24
              Hint = 'show_classindex_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
          object pnlViewSearchResult: TPanel
            Left = 0
            Height = 40
            Top = 40
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 1
            object lblViewSearchResult: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'Switch Class Search Result'
              Layout = tlCenter
              ParentColor = False
            end
            object btnViewSearchResult: TBitBtn
              Left = 444
              Height = 24
              Hint = 'show_searchresult_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
          object pnlViewConsole: TPanel
            Left = 0
            Height = 40
            Top = 80
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 2
            object lblViewConsole: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'Switch Console'
              Layout = tlCenter
              ParentColor = False
            end
            object btnViewConsole: TBitBtn
              Left = 444
              Height = 24
              Hint = 'show_console_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
          object pnlViewCloseAllPages: TPanel
            Left = 0
            Height = 40
            Top = 160
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 3
            object lblViewCloseAllPages: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'Close All Pages'
              Layout = tlCenter
              ParentColor = False
            end
            object btnViewCloseAllPages: TBitBtn
              Left = 444
              Height = 24
              Hint = 'close_all_pages_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
          object pnlViewCloseAllOtherPages: TPanel
            Left = 0
            Height = 40
            Top = 200
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 4
            object lblViewCloseAllOtherPages: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'Close All Other Pages'
              Layout = tlCenter
              ParentColor = False
            end
            object btnViewCloseAllOtherPages: TBitBtn
              Left = 444
              Height = 24
              Hint = 'close_all_other_pages_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
          object pnlViewSsmali: TPanel
            Left = 0
            Height = 40
            Top = 120
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 5
            object lblViewSsmali: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'Switch Ssmali'
              Layout = tlCenter
              ParentColor = False
            end
            object btnViewSsmali: TBitBtn
              Left = 444
              Height = 24
              Hint = 'show_ssmali_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
        end
        object gbPackage: TGroupBox
          Left = 8
          Height = 144
          Top = 280
          Width = 656
          Align = alTop
          BorderSpacing.Around = 8
          Caption = 'Package'
          ClientHeight = 125
          ClientWidth = 652
          TabOrder = 1
          object pnlPackageDecompile: TPanel
            Left = 0
            Height = 40
            Top = 0
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 0
            object lblPackageDecompile: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'Decompile'
              Layout = tlCenter
              ParentColor = False
            end
            object btnPackageDecompile: TBitBtn
              Left = 444
              Height = 24
              Hint = 'decompile_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
          object pnlPackageCompile: TPanel
            Left = 0
            Height = 40
            Top = 40
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 1
            object lblPackageCompile: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'Compile'
              Layout = tlCenter
              ParentColor = False
            end
            object btnPackageCompile: TBitBtn
              Left = 444
              Height = 24
              Hint = 'compile_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
          object pnlPackageInstallFramework: TPanel
            Left = 0
            Height = 40
            Top = 80
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 2
            object lblPackageInstallFramework: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'Install Framework'
              Layout = tlCenter
              ParentColor = False
            end
            object btnPackageInstallFramework: TBitBtn
              Left = 444
              Height = 24
              Hint = 'install_framework_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
        end
        object gbCodeTree: TGroupBox
          Left = 8
          Height = 264
          Top = 432
          Width = 656
          Align = alTop
          BorderSpacing.Around = 8
          Caption = 'CodeTree'
          ClientHeight = 245
          ClientWidth = 652
          TabOrder = 2
          object pnlCodeTreeNewClass: TPanel
            Left = 0
            Height = 40
            Top = 0
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 0
            object lblCodeTreeNewClass: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'New Class'
              Layout = tlCenter
              ParentColor = False
            end
            object btnCodeTreeNewClass: TBitBtn
              Left = 444
              Height = 24
              Hint = 'new_class_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
          object pnlCodetreeNewInterface: TPanel
            Left = 0
            Height = 40
            Top = 40
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 1
            object lblCodeTreeNewInterface: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'New Interface'
              Layout = tlCenter
              ParentColor = False
            end
            object btnCodeTreeNewInterface: TBitBtn
              Left = 444
              Height = 24
              Hint = 'new_interface_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
          object pnlCodeTreeNewEnum: TPanel
            Left = 0
            Height = 40
            Top = 80
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 2
            object lblCodeTreeNewEnum: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'New Enum'
              Layout = tlCenter
              ParentColor = False
            end
            object btnCodeTreeNewEnum: TBitBtn
              Left = 444
              Height = 24
              Hint = 'new_enum_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
          object pnlCodeTreeDeleteFile: TPanel
            Left = 0
            Height = 40
            Top = 200
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 3
            object lblCodeTreeDeleteFile: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'Delete File'
              Layout = tlCenter
              ParentColor = False
            end
            object btnCodeTreeDeleteFile: TBitBtn
              Left = 444
              Height = 24
              Hint = 'delete_file_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
          object pnlCodeTreeNewTextFile: TPanel
            Left = 0
            Height = 40
            Top = 160
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 4
            object lblCodeTreeNewTextFile: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'New Text File'
              Layout = tlCenter
              ParentColor = False
            end
            object btnCodeTreeNewTextFile: TBitBtn
              Left = 444
              Height = 24
              Hint = 'new_textfile_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
          object pnlCodeTreeNewAnnotation: TPanel
            Left = 0
            Height = 40
            Top = 120
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 5
            object lblCodeTreeNewAnnotation: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'New Annotation'
              Layout = tlCenter
              ParentColor = False
            end
            object btnCodeTreeNewAnnotation: TBitBtn
              Left = 444
              Height = 24
              Hint = 'new_annotation_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
        end
        object gbCodeEditor: TGroupBox
          Left = 8
          Height = 224
          Top = 704
          Width = 656
          Align = alTop
          BorderSpacing.Around = 8
          Caption = 'CodeEditor'
          ClientHeight = 205
          ClientWidth = 652
          TabOrder = 3
          object pnlCodeEditorJumpClassMethod: TPanel
            Left = 0
            Height = 40
            Top = 0
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 0
            object lblCodeEditorJumpClassMethod: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'Jump Class / Method'
              Layout = tlCenter
              ParentColor = False
            end
            object btnCodeEditorJumpClassMethod: TBitBtn
              Left = 444
              Height = 24
              Hint = 'jump_class_method_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
          object pnlCodeEditorSmaliToJava: TPanel
            Left = 0
            Height = 40
            Top = 40
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 1
            object lblCodeEditorSmaliToJava: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'Smali to Java'
              Layout = tlCenter
              ParentColor = False
            end
            object btnCodeEditorSmaliToJava: TBitBtn
              Left = 444
              Height = 24
              Hint = 'jump_to_java_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
          object pnlCodeEditorKeywordsCompletion: TPanel
            Left = 0
            Height = 40
            Top = 80
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 2
            object lblCodeEditorKeywordsCompletion: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'Keywords Completion'
              Layout = tlCenter
              ParentColor = False
            end
            object btnCodeEditorKeywordsCompletion: TBitBtn
              Left = 444
              Height = 24
              Hint = 'hint_keyword_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
          object pnlCodeEditorTemplateCompletion: TPanel
            Left = 0
            Height = 40
            Top = 160
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 3
            object lblCodeEditorTemplateCompletion: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'Template Completion'
              Layout = tlCenter
              ParentColor = False
            end
            object btnCodeEditorTemplateCompletion: TBitBtn
              Left = 444
              Height = 24
              Hint = 'hint_template_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
          object pnlCodeEditorClassMethodCompletion: TPanel
            Left = 0
            Height = 40
            Top = 120
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 4
            object lblCodeEditorClassMethodCompletion: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'Class / Method Completion'
              Layout = tlCenter
              ParentColor = False
            end
            object btnCodeEditorClassMethodCompletion: TBitBtn
              Left = 444
              Height = 24
              Hint = 'hint_classmethod_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
        end
        object gbHelp: TGroupBox
          Left = 8
          Height = 64
          Top = 936
          Width = 656
          Align = alTop
          BorderSpacing.Around = 8
          Caption = 'Help'
          ClientHeight = 45
          ClientWidth = 652
          TabOrder = 4
          object pnlHelpSettings: TPanel
            Left = 0
            Height = 40
            Top = 0
            Width = 652
            Align = alTop
            BevelOuter = bvNone
            ClientHeight = 40
            ClientWidth = 652
            TabOrder = 0
            object lblHelpSettings: TLabel
              Left = 8
              Height = 24
              Top = 8
              Width = 250
              Align = alLeft
              AutoSize = False
              BorderSpacing.Around = 8
              Caption = 'Settings'
              Layout = tlCenter
              ParentColor = False
            end
            object btnHelpSettings: TBitBtn
              Left = 444
              Height = 24
              Hint = 'settings_shortcut'
              Top = 8
              Width = 200
              Align = alRight
              BorderSpacing.Around = 8
              Caption = '(none)'
              OnClick = btnViewClassIndexClick
              TabOrder = 0
            end
          end
        end
      end
    end
    object tsStyle: TTabSheet
      Caption = 'Style'
      ClientHeight = 588
      ClientWidth = 688
      object gbFont: TGroupBox
        Left = 8
        Height = 113
        Top = 8
        Width = 672
        Align = alTop
        BorderSpacing.Around = 8
        Caption = 'Font'
        ClientHeight = 94
        ClientWidth = 668
        TabOrder = 0
        object pnlFont: TPanel
          Left = 0
          Height = 44
          Top = 0
          Width = 668
          Align = alTop
          BevelOuter = bvNone
          ClientHeight = 44
          ClientWidth = 668
          TabOrder = 0
          object btnFontName: TBitBtn
            Left = 632
            Height = 28
            Top = 8
            Width = 28
            Align = alRight
            BorderSpacing.Around = 8
            Caption = '...'
            OnClick = btnFontNameClick
            TabOrder = 0
          end
          object lblFontName: TLabel
            Left = 8
            Height = 28
            Top = 8
            Width = 80
            Align = alLeft
            AutoSize = False
            BorderSpacing.Around = 8
            Caption = 'Name'
            Layout = tlCenter
            ParentColor = False
          end
          object lblFontNameValue: TLabel
            Left = 96
            Height = 28
            Top = 8
            Width = 150
            Align = alLeft
            AutoSize = False
            BorderSpacing.Around = 8
            Caption = 'Default'
            Layout = tlCenter
            ParentColor = False
          end
          object lblFontSize: TLabel
            Left = 270
            Height = 28
            Top = 8
            Width = 80
            Align = alLeft
            AutoSize = False
            BorderSpacing.Left = 16
            BorderSpacing.Around = 8
            Caption = 'Size'
            Layout = tlCenter
            ParentColor = False
          end
          object lblFontSizeValue: TLabel
            Left = 358
            Height = 28
            Top = 8
            Width = 80
            Align = alLeft
            AutoSize = False
            BorderSpacing.Around = 8
            Caption = '10'
            Layout = tlCenter
            ParentColor = False
          end
        end
        object pnlFontAntiAlias: TPanel
          Left = 0
          Height = 44
          Top = 44
          Width = 668
          Align = alTop
          BevelOuter = bvNone
          ClientHeight = 44
          ClientWidth = 668
          TabOrder = 1
          object chkFontAntiAliasing: TCheckBox
            Left = 8
            Height = 28
            Top = 8
            Width = 120
            Align = alLeft
            AutoSize = False
            BorderSpacing.Around = 8
            Caption = 'Anti-Aliasing'
            OnClick = chkFontAntiAliasingClick
            TabOrder = 0
          end
        end
      end
      object gbUI: TGroupBox
        Left = 8
        Height = 151
        Top = 129
        Width = 672
        Align = alTop
        BorderSpacing.Around = 8
        Caption = 'UI'
        ClientHeight = 132
        ClientWidth = 668
        TabOrder = 1
        object pnlTrans: TPanel
          Left = 0
          Height = 44
          Top = 0
          Width = 668
          Align = alTop
          BevelOuter = bvNone
          ClientHeight = 44
          ClientWidth = 668
          TabOrder = 0
          object chkUITrans: TCheckBox
            Left = 8
            Height = 28
            Top = 8
            Width = 120
            Align = alLeft
            AutoSize = False
            BorderSpacing.Around = 8
            Caption = 'Transparent'
            OnClick = chkUITransClick
            TabOrder = 0
          end
          object trackTrans: TTrackBar
            Left = 136
            Height = 46
            Top = 0
            Width = 524
            Max = 255
            Min = 100
            OnChange = trackTransChange
            PageSize = 1
            Position = 255
            ScalePos = trRight
            ShowSelRange = False
            TickStyle = tsManual
            Align = alClient
            BorderSpacing.Left = 8
            BorderSpacing.Right = 8
            TabOrder = 1
          end
        end
        object pnlIColor: TPanel
          Left = 0
          Height = 44
          Top = 44
          Width = 668
          Align = alTop
          BevelOuter = bvNone
          ClientHeight = 44
          ClientWidth = 668
          TabOrder = 1
          object lblUIColor: TLabel
            Left = 8
            Height = 28
            Top = 8
            Width = 80
            Align = alLeft
            AutoSize = False
            BorderSpacing.Around = 8
            Caption = 'Color'
            Layout = tlCenter
            ParentColor = False
          end
          object pnlColorValue: TPanel
            Left = 112
            Height = 28
            Top = 8
            Width = 170
            Align = alLeft
            BorderSpacing.Left = 16
            BorderSpacing.Around = 8
            BevelInner = bvRaised
            BevelOuter = bvLowered
            Color = clWhite
            ParentColor = False
            TabOrder = 0
          end
          object btnUIColor: TBitBtn
            Left = 632
            Height = 28
            Top = 8
            Width = 28
            Align = alRight
            BorderSpacing.Around = 8
            Caption = '...'
            Color = clWhite
            OnClick = btnUIColorClick
            TabOrder = 1
          end
        end
        object pnlIFontColor: TPanel
          Left = 0
          Height = 44
          Top = 88
          Width = 668
          Align = alTop
          BevelOuter = bvNone
          ClientHeight = 44
          ClientWidth = 668
          TabOrder = 2
          object lblUIFontColor: TLabel
            Left = 8
            Height = 28
            Top = 8
            Width = 80
            Align = alLeft
            AutoSize = False
            BorderSpacing.Around = 8
            Caption = 'Font Color'
            Layout = tlCenter
            ParentColor = False
          end
          object pnlFontColorValue: TPanel
            Left = 112
            Height = 28
            Top = 8
            Width = 170
            Align = alLeft
            BorderSpacing.Left = 16
            BorderSpacing.Around = 8
            BevelInner = bvRaised
            BevelOuter = bvLowered
            Color = clWhite
            ParentColor = False
            TabOrder = 0
          end
          object btnUIFontColor: TBitBtn
            Left = 632
            Height = 28
            Top = 8
            Width = 28
            Align = alRight
            BorderSpacing.Around = 8
            Caption = '...'
            OnClick = btnUIFontColorClick
            TabOrder = 1
          end
        end
      end
    end
    object tsHighlight: TTabSheet
      Caption = 'Highlight'
      ClientHeight = 588
      ClientWidth = 688
      object lstStyles: TListBox
        Left = 8
        Height = 572
        Top = 8
        Width = 192
        Align = alLeft
        BorderSpacing.Left = 8
        BorderSpacing.Top = 8
        BorderSpacing.Bottom = 8
        Color = clWhite
        ItemHeight = 0
        OnClick = lstStylesClick
        ParentFont = False
        ScrollWidth = 190
        TabOrder = 0
        TopIndex = -1
      end
      object splStyle: TSplitter
        Left = 200
        Height = 588
        Top = 0
        Width = 3
      end
      object pgStyles: TPageControl
        Left = 203
        Height = 572
        Top = 8
        Width = 477
        ActivePage = tsSmali
        Align = alClient
        BorderSpacing.Top = 8
        BorderSpacing.Right = 8
        BorderSpacing.Bottom = 8
        TabIndex = 0
        TabOrder = 2
        object tsSmali: TTabSheet
          Caption = 'Smali'
        end
        object tsJava: TTabSheet
          Caption = 'Java'
        end
        object tsXML: TTabSheet
          Caption = 'XML'
        end
        object tsHTML: TTabSheet
          Caption = 'HTML'
        end
        object tsJs: TTabSheet
          Caption = 'Js'
        end
        object tsCSS: TTabSheet
          Caption = 'CSS'
        end
        object tsShell: TTabSheet
          Caption = 'Shell'
        end
      end
    end
    object tsFileType: TTabSheet
      Caption = 'FileType'
      ClientHeight = 588
      ClientWidth = 688
      object sbxFileType: TScrollBox
        Left = 0
        Height = 548
        Top = 0
        Width = 688
        HorzScrollBar.Page = 1
        VertScrollBar.Page = 1
        Align = alClient
        BorderStyle = bsNone
        TabOrder = 0
      end
      object pnlFileTypeBtn: TPanel
        Left = 0
        Height = 40
        Top = 548
        Width = 688
        Align = alBottom
        BevelOuter = bvNone
        ClientHeight = 40
        ClientWidth = 688
        TabOrder = 1
        object btnAddFileType: TBitBtn
          Left = 8
          Height = 24
          Top = 8
          Width = 60
          Align = alLeft
          BorderSpacing.Around = 8
          Caption = 'Add'
          OnClick = btnAddFileTypeClick
          TabOrder = 0
        end
      end
    end
    object tsTemplate: TTabSheet
      Caption = 'Template'
      ClientHeight = 588
      ClientWidth = 688
      object pnlTemplateList: TPanel
        Left = 8
        Height = 572
        Top = 8
        Width = 192
        Align = alLeft
        BorderSpacing.Left = 8
        BorderSpacing.Top = 8
        BorderSpacing.Bottom = 8
        BevelOuter = bvNone
        ClientHeight = 572
        ClientWidth = 192
        TabOrder = 0
        object lstTemplate: TListBox
          Left = 0
          Height = 524
          Top = 0
          Width = 192
          Align = alClient
          ItemHeight = 0
          OnClick = lstTemplateClick
          ScrollWidth = 190
          TabOrder = 0
          TopIndex = -1
        end
        object pnlTemplateOperation: TPanel
          Left = 0
          Height = 48
          Top = 524
          Width = 192
          Align = alBottom
          BevelOuter = bvNone
          ClientHeight = 48
          ClientWidth = 192
          TabOrder = 1
          object btnAddTemplate: TBitBtn
            Left = 8
            Height = 32
            Top = 8
            Width = 60
            Align = alLeft
            BorderSpacing.Around = 8
            Caption = 'Add'
            OnClick = btnAddTemplateClick
            TabOrder = 0
          end
          object btnDeleteTemplate: TBitBtn
            Left = 76
            Height = 32
            Top = 8
            Width = 60
            Align = alLeft
            BorderSpacing.Around = 8
            Caption = 'Delete'
            OnClick = btnDeleteTemplateClick
            TabOrder = 1
          end
        end
      end
      object splTemplateList: TSplitter
        Left = 200
        Height = 588
        Top = 0
        Width = 3
      end
    end
  end
end
