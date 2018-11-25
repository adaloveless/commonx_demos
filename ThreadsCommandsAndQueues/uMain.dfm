object Form1: TForm1
  Left = 0
  Top = 0
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
    Left = 378
    Top = 0
    Width = 8
    Height = 582
    ExplicitLeft = 289
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 378
    Height = 582
    ActivePage = TabSheet2
    Align = alLeft
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Queues'
      OnContextPopup = TabSheet1ContextPopup
      object lblResult: TLabel
        Left = 32
        Top = 96
        Width = 321
        Height = 153
        AutoSize = False
        Caption = 'Results:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
        OnClick = lblResultClick
      end
      object btnQueues: TButton
        Left = 16
        Top = 24
        Width = 233
        Height = 49
        Caption = 'Test Primes using  Queues'
        TabOrder = 0
        OnClick = btnQueuesClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Commands'
      ImageIndex = 1
      object lblResult2: TLabel
        Left = 32
        Top = 96
        Width = 321
        Height = 153
        AutoSize = False
        Caption = 'Results:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
        OnClick = lblResultClick
      end
      object btnCommands: TButton
        Left = 16
        Top = 24
        Width = 233
        Height = 49
        Caption = 'Test Primes using Commands'
        TabOrder = 0
        OnClick = btnCommandsClick
      end
    end
  end
  object panFrameHost: TPanel
    Left = 386
    Top = 0
    Width = 471
    Height = 582
    Align = alClient
    TabOrder = 1
  end
  object tmCheckCommand: TTimer
    OnTimer = tmCheckCommandTimer
    Left = 200
    Top = 368
  end
end
