object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 297
  ClientWidth = 394
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblLabel: TLabel
    Left = 24
    Top = 64
    Width = 35
    Height = 13
    Caption = 'lblLabel'
  end
  object Label1: TLabel
    Left = 138
    Top = 64
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object btnUpdateLabel: TButton
    Left = 24
    Top = 17
    Width = 185
    Height = 25
    Caption = 'update label after 10 seconds'
    TabOrder = 0
    OnClick = btnUpdateLabelClick
  end
  object btnCatchPromise: TButton
    Left = 24
    Top = 120
    Width = 185
    Height = 25
    Caption = 'Promise with catch'
    TabOrder = 1
    OnClick = btnCatchPromiseClick
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 232
    Top = 8
  end
end
