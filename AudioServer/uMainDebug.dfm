object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 506
  ClientWidth = 1034
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object udps: TIdUDPServer
    BufferSize = 16384
    BroadcastEnabled = True
    Bindings = <>
    DefaultPort = 788
    ThreadedEvent = True
    Left = 152
    Top = 240
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 184
    Top = 168
  end
  object Timer2: TTimer
    Interval = 4000
    OnTimer = Timer2Timer
    Left = 379
    Top = 160
  end
  object Timer3: TTimer
    Interval = 100
    OnTimer = Timer3Timer
    Left = 360
    Top = 256
  end
  object IdIPMCastClient1: TIdIPMCastClient
    Bindings = <>
    DefaultPort = 787
    MulticastGroup = '224.0.0.1'
    ReuseSocket = rsTrue
    ThreadedEvent = True
    Left = 216
    Top = 352
  end
end
