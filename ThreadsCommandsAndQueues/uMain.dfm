object Form1: TForm1
  Left = 0
  Top = 0
  Align = alLeft
  Caption = 'Form1'
  ClientHeight = 582
  ClientWidth = 857
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 289
    Top = 0
    Width = 8
    Height = 582
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 289
    Height = 582
    ActivePage = TabSheet1
    Align = alLeft
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Queues'
      ExplicitHeight = 165
      object lblResult: TLabel
        Left = 32
        Top = 96
        Width = 73
        Height = 25
        Caption = 'Results:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Button1: TButton
        Left = 16
        Top = 24
        Width = 233
        Height = 49
        Caption = 'Test Primes using  Queues'
        TabOrder = 0
        OnClick = Button1Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Commands'
      ImageIndex = 1
      ExplicitHeight = 165
    end
  end
  object panFrameHost: TPanel
    Left = 297
    Top = 0
    Width = 560
    Height = 582
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 344
    ExplicitTop = 288
    ExplicitWidth = 185
    ExplicitHeight = 41
  end
end
