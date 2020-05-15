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
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object lblLabel: TLabel
    Left = 24
    Top = 64
    Width = 35
    Height = 13
    Caption = 'lblLabel'
  end
  object lblLabel2: TLabel
    Left = 24
    Top = 136
    Width = 35
    Height = 13
    Caption = 'lblLabel'
  end
  object lblPromiseState: TLabel
    Left = 273
    Top = 79
    Width = 73
    Height = 13
    Caption = 'lblPromiseState'
  end
  object Label1: TLabel
    Left = 138
    Top = 64
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object lblAnonPromiseValue: TLabel
    Left = 24
    Top = 216
    Width = 35
    Height = 13
    Caption = 'lblLabel'
  end
  object lblAnonPromiseTimer: TLabel
    Left = 138
    Top = 216
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
  object btnUpdateLabel2: TButton
    Left = 24
    Top = 97
    Width = 185
    Height = 25
    Caption = 'update next label with first promise'
    TabOrder = 1
    OnClick = btnUpdateLabel2Click
  end
  object btnPromiseState: TButton
    Left = 273
    Top = 48
    Width = 107
    Height = 25
    Caption = 'Show promise state'
    TabOrder = 2
    OnClick = btnPromiseStateClick
  end
  object btnPromiseRejection: TButton
    Left = 24
    Top = 248
    Width = 145
    Height = 25
    Caption = 'Promise with reject'
    TabOrder = 3
    OnClick = btnPromiseRejectionClick
  end
  object btnCancelPromise: TButton
    Left = 273
    Top = 17
    Width = 113
    Height = 25
    Caption = 'cancel promise'
    TabOrder = 4
    OnClick = btnCancelPromiseClick
  end
  object btnUpdateWithAnonPromise: TButton
    Left = 24
    Top = 169
    Width = 305
    Height = 25
    Caption = 'update label after 10 seconds (anonymous promise)'
    TabOrder = 5
    OnClick = btnUpdateWithAnonPromiseClick
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 232
    Top = 8
  end
  object Timer2: TTimer
    Enabled = False
    OnTimer = Timer2Timer
    Left = 208
    Top = 200
  end
end
