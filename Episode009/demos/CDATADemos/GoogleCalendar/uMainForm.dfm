object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Google Calendar'
  ClientHeight = 483
  ClientWidth = 734
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 734
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 16
      Top = 13
      Width = 75
      Height = 25
      Caption = 'Open'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 176
      Top = 13
      Width = 75
      Height = 25
      Caption = 'Create'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 97
      Top = 13
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 2
      OnClick = Button3Click
    end
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 49
    Width = 734
    Height = 409
    Align = alClient
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'Summary'
        Width = 155
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'StartDateTime'
        Width = 155
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'EndDateTime'
        Width = 155
        Visible = True
      end>
  end
  object DBNavigator1: TDBNavigator
    Left = 0
    Top = 458
    Width = 734
    Height = 25
    DataSource = DataSource1
    Align = alBottom
    Flat = True
    TabOrder = 2
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'InitiateOAuth=GETANDREFRESH'
      'DriverID=CDataGoogleCalendar')
    LoginPrompt = False
    Left = 600
    Top = 72
  end
  object FDPhysCDataGoogleCalendarDriverLink1: TFDPhysCDataGoogleCalendarDriverLink
    DriverID = 'CDataGoogleCalendar'
    Left = 600
    Top = 128
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'Select * FROM "fernando.rizzato@gmail.com" '
      'WHERE StartDateTime IS NOT NULL'
      'ORDER BY StartDateTime Desc ')
    Left = 600
    Top = 192
    object FDQuery1Id: TWideStringField
      FieldName = 'Id'
      Origin = 'Id'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      ReadOnly = True
      Required = True
      Size = 255
    end
    object FDQuery1CalendarId: TWideStringField
      FieldName = 'CalendarId'
      Origin = 'CalendarId'
      Required = True
      Size = 2000
    end
    object FDQuery1Summary: TWideStringField
      FieldName = 'Summary'
      Origin = 'Summary'
      Size = 2000
    end
    object FDQuery1Description: TWideStringField
      FieldName = 'Description'
      Origin = 'Description'
      Size = 2000
    end
    object FDQuery1Location: TWideStringField
      FieldName = 'Location'
      Origin = 'Location'
      Size = 2000
    end
    object FDQuery1AllDayEvent: TBooleanField
      FieldName = 'AllDayEvent'
      Origin = 'AllDayEvent'
      ReadOnly = True
    end
    object FDQuery1StartDate: TDateField
      FieldName = 'StartDate'
      Origin = 'StartDate'
    end
    object FDQuery1StartDateTime: TSQLTimeStampField
      FieldName = 'StartDateTime'
      Origin = 'StartDateTime'
    end
    object FDQuery1StartDateTimeZone: TWideStringField
      FieldName = 'StartDateTimeZone'
      Origin = 'StartDateTimeZone'
      Size = 2000
    end
    object FDQuery1EndDate: TDateField
      FieldName = 'EndDate'
      Origin = 'EndDate'
    end
    object FDQuery1EndDateTime: TSQLTimeStampField
      FieldName = 'EndDateTime'
      Origin = 'EndDateTime'
    end
    object FDQuery1EndDateTimeZone: TWideStringField
      FieldName = 'EndDateTimeZone'
      Origin = 'EndDateTimeZone'
      Size = 2000
    end
    object FDQuery1OriginalStartTimeDateTime: TSQLTimeStampField
      FieldName = 'OriginalStartTimeDateTime'
      Origin = 'OriginalStartTimeDateTime'
    end
    object FDQuery1SendNotification: TBooleanField
      FieldName = 'SendNotification'
      Origin = 'SendNotification'
    end
    object FDQuery1Kind: TWideStringField
      FieldName = 'Kind'
      Origin = 'Kind'
      ReadOnly = True
      Required = True
      Size = 2000
    end
    object FDQuery1ETag: TWideStringField
      FieldName = 'ETag'
      Origin = 'ETag'
      ReadOnly = True
      Required = True
      Size = 2000
    end
    object FDQuery1Status: TWideStringField
      FieldName = 'Status'
      Origin = 'Status'
      Size = 2000
    end
    object FDQuery1HTML_Link: TWideStringField
      FieldName = 'HTML_Link'
      Origin = 'HTML_Link'
      ReadOnly = True
      Required = True
      Size = 2000
    end
    object FDQuery1Locked: TBooleanField
      FieldName = 'Locked'
      Origin = 'Locked'
      ReadOnly = True
    end
    object FDQuery1SourceTitle: TWideStringField
      FieldName = 'SourceTitle'
      Origin = 'SourceTitle'
      Size = 2000
    end
    object FDQuery1SourceURL: TWideStringField
      FieldName = 'SourceURL'
      Origin = 'SourceURL'
      Size = 2000
    end
    object FDQuery1HangoutLink: TWideStringField
      FieldName = 'HangoutLink'
      Origin = 'HangoutLink'
      ReadOnly = True
      Size = 2000
    end
    object FDQuery1Created: TSQLTimeStampField
      FieldName = 'Created'
      Origin = 'Created'
      ReadOnly = True
    end
    object FDQuery1Updated: TSQLTimeStampField
      FieldName = 'Updated'
      Origin = 'Updated'
      ReadOnly = True
    end
    object FDQuery1ColorId: TIntegerField
      FieldName = 'ColorId'
      Origin = 'ColorId'
    end
    object FDQuery1CreatorEmail: TWideStringField
      FieldName = 'CreatorEmail'
      Origin = 'CreatorEmail'
      ReadOnly = True
      Required = True
      Size = 2000
    end
    object FDQuery1CreatorDisplayName: TWideStringField
      FieldName = 'CreatorDisplayName'
      Origin = 'CreatorDisplayName'
      ReadOnly = True
      Required = True
      Size = 2000
    end
    object FDQuery1OrganizerEmail: TWideStringField
      FieldName = 'OrganizerEmail'
      Origin = 'OrganizerEmail'
      Required = True
      Size = 2000
    end
    object FDQuery1OrganizerDisplayName: TWideStringField
      FieldName = 'OrganizerDisplayName'
      Origin = 'OrganizerDisplayName'
      Size = 2000
    end
    object FDQuery1Recurrences: TWideStringField
      FieldName = 'Recurrences'
      Origin = 'Recurrences'
      Size = 2000
    end
    object FDQuery1RecurringEventId: TWideStringField
      FieldName = 'RecurringEventId'
      Origin = 'RecurringEventId'
      ReadOnly = True
      Required = True
      Size = 2000
    end
    object FDQuery1Transparency: TWideStringField
      FieldName = 'Transparency'
      Origin = 'Transparency'
      Size = 2000
    end
    object FDQuery1Visibility: TWideStringField
      FieldName = 'Visibility'
      Origin = 'Visibility'
      Size = 2000
    end
    object FDQuery1ICalUid: TWideStringField
      FieldName = 'ICalUid'
      Origin = 'ICalUid'
      ReadOnly = True
      Required = True
      Size = 2000
    end
    object FDQuery1Sequence: TWideStringField
      FieldName = 'Sequence'
      Origin = 'Sequence'
      Required = True
      Size = 2000
    end
    object FDQuery1AttendeesEmails: TWideStringField
      FieldName = 'AttendeesEmails'
      Origin = 'AttendeesEmails'
      Size = 2000
    end
    object FDQuery1AttendeesDisplayNames: TWideStringField
      FieldName = 'AttendeesDisplayNames'
      Origin = 'AttendeesDisplayNames'
      Size = 2000
    end
    object FDQuery1AttendeesResponseStatus: TWideStringField
      FieldName = 'AttendeesResponseStatus'
      Origin = 'AttendeesResponseStatus'
      Size = 2000
    end
    object FDQuery1AttendeesOmitted: TBooleanField
      FieldName = 'AttendeesOmitted'
      Origin = 'AttendeesOmitted'
      ReadOnly = True
    end
    object FDQuery1ExtendedPropertiesPrivateKey: TWideStringField
      FieldName = 'ExtendedPropertiesPrivateKey'
      Origin = 'ExtendedPropertiesPrivateKey'
      Size = 2000
    end
    object FDQuery1ExtendedPropertiesPrivateValue: TWideStringField
      FieldName = 'ExtendedPropertiesPrivateValue'
      Origin = 'ExtendedPropertiesPrivateValue'
      Size = 2000
    end
    object FDQuery1ExtendedPropertiesSharedKey: TWideStringField
      FieldName = 'ExtendedPropertiesSharedKey'
      Origin = 'ExtendedPropertiesSharedKey'
      Size = 2000
    end
    object FDQuery1ExtendedPropertiesSharedValue: TWideStringField
      FieldName = 'ExtendedPropertiesSharedValue'
      Origin = 'ExtendedPropertiesSharedValue'
      Size = 2000
    end
    object FDQuery1AnyoneCanAddSelf: TBooleanField
      FieldName = 'AnyoneCanAddSelf'
      Origin = 'AnyoneCanAddSelf'
      ReadOnly = True
    end
    object FDQuery1GuestsCanInviteOthers: TBooleanField
      FieldName = 'GuestsCanInviteOthers'
      Origin = 'GuestsCanInviteOthers'
    end
    object FDQuery1GuestsCanSeeOtherGuests: TBooleanField
      FieldName = 'GuestsCanSeeOtherGuests'
      Origin = 'GuestsCanSeeOtherGuests'
    end
    object FDQuery1GuestsCanModify: TBooleanField
      FieldName = 'GuestsCanModify'
      Origin = 'GuestsCanModify'
    end
    object FDQuery1PrivateCopy: TBooleanField
      FieldName = 'PrivateCopy'
      Origin = 'PrivateCopy'
      ReadOnly = True
      Required = True
    end
    object FDQuery1RemindersUseDefault: TBooleanField
      FieldName = 'RemindersUseDefault'
      Origin = 'RemindersUseDefault'
      Required = True
    end
    object FDQuery1ReminderOverrideMethods: TWideStringField
      FieldName = 'ReminderOverrideMethods'
      Origin = 'ReminderOverrideMethods'
      Size = 2000
    end
    object FDQuery1ReminderOverrideMinutes: TWideStringField
      FieldName = 'ReminderOverrideMinutes'
      Origin = 'ReminderOverrideMinutes'
      Size = 2000
    end
    object FDQuery1SearchTerms: TWideStringField
      FieldName = 'SearchTerms'
      Origin = 'SearchTerms'
      ReadOnly = True
      Required = True
      Size = 2000
    end
    object FDQuery1SingleEvents: TWideStringField
      FieldName = 'SingleEvents'
      Origin = 'SingleEvents'
      ReadOnly = True
      Required = True
      Size = 2000
    end
    object FDQuery1ConferenceId: TWideStringField
      FieldName = 'ConferenceId'
      Origin = 'ConferenceId'
      Size = 2000
    end
    object FDQuery1ConferenceNotes: TWideStringField
      FieldName = 'ConferenceNotes'
      Origin = 'ConferenceNotes'
      Size = 2000
    end
    object FDQuery1ConferenceSignature: TWideStringField
      FieldName = 'ConferenceSignature'
      Origin = 'ConferenceSignature'
      Size = 2000
    end
    object FDQuery1ConferenceSolutionName: TWideStringField
      FieldName = 'ConferenceSolutionName'
      Origin = 'ConferenceSolutionName'
      Size = 2000
    end
    object FDQuery1ConferenceSolutionKeyType: TWideStringField
      FieldName = 'ConferenceSolutionKeyType'
      Origin = 'ConferenceSolutionKeyType'
      Size = 2000
    end
    object FDQuery1ConferenceSolutionIconUri: TWideStringField
      FieldName = 'ConferenceSolutionIconUri'
      Origin = 'ConferenceSolutionIconUri'
      Size = 2000
    end
    object FDQuery1ConferenceRequestId: TWideStringField
      FieldName = 'ConferenceRequestId'
      Origin = 'ConferenceRequestId'
      Size = 2000
    end
    object FDQuery1ConferenceRequestKey: TWideStringField
      FieldName = 'ConferenceRequestKey'
      Origin = 'ConferenceRequestKey'
      Size = 2000
    end
    object FDQuery1ConferenceRequestKeyType: TWideStringField
      FieldName = 'ConferenceRequestKeyType'
      Origin = 'ConferenceRequestKeyType'
      Size = 2000
    end
    object FDQuery1ConferenceRequestStatus: TWideStringField
      FieldName = 'ConferenceRequestStatus'
      Origin = 'ConferenceRequestStatus'
      Size = 2000
    end
    object FDQuery1ConferenceRequestStatusCode: TWideStringField
      FieldName = 'ConferenceRequestStatusCode'
      Origin = 'ConferenceRequestStatusCode'
      ReadOnly = True
      Size = 2000
    end
    object FDQuery1ConferenceEntryPointsAggregate: TWideStringField
      FieldName = 'ConferenceEntryPointsAggregate'
      Origin = 'ConferenceEntryPointsAggregate'
      Size = 2000
    end
    object FDQuery1GadgetTitle: TWideStringField
      FieldName = 'GadgetTitle'
      Origin = 'GadgetTitle'
      Size = 2000
    end
    object FDQuery1GadgetType: TWideStringField
      FieldName = 'GadgetType'
      Origin = 'GadgetType'
      Size = 2000
    end
    object FDQuery1GadgetDisplay: TWideStringField
      FieldName = 'GadgetDisplay'
      Origin = 'GadgetDisplay'
      Size = 2000
    end
    object FDQuery1GadgetHeight: TIntegerField
      FieldName = 'GadgetHeight'
      Origin = 'GadgetHeight'
    end
    object FDQuery1GadgetWidth: TIntegerField
      FieldName = 'GadgetWidth'
      Origin = 'GadgetWidth'
    end
    object FDQuery1GadgetIconLink: TWideStringField
      FieldName = 'GadgetIconLink'
      Origin = 'GadgetIconLink'
      Size = 2000
    end
    object FDQuery1GadgetLink: TWideStringField
      FieldName = 'GadgetLink'
      Origin = 'GadgetLink'
      Size = 2000
    end
    object FDQuery1EventType: TWideStringField
      FieldName = 'EventType'
      Origin = 'EventType'
      Size = 2000
    end
  end
  object DataSource1: TDataSource
    DataSet = FDQuery1
    Left = 600
    Top = 256
  end
end
