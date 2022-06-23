unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.CDataGoogleCalendar, FireDAC.Phys.CDataGoogleCalendarDef,
  FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client, Vcl.StdCtrls,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  Vcl.DBCtrls, Vcl.Grids, Vcl.DBGrids, FireDAC.Comp.DataSet, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    FDConnection1: TFDConnection;
    FDPhysCDataGoogleCalendarDriverLink1: TFDPhysCDataGoogleCalendarDriverLink;
    Panel1: TPanel;
    Button1: TButton;
    FDQuery1: TFDQuery;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Button2: TButton;
    Button3: TButton;
    FDQuery1Id: TWideStringField;
    FDQuery1CalendarId: TWideStringField;
    FDQuery1Summary: TWideStringField;
    FDQuery1Description: TWideStringField;
    FDQuery1Location: TWideStringField;
    FDQuery1AllDayEvent: TBooleanField;
    FDQuery1StartDate: TDateField;
    FDQuery1StartDateTime: TSQLTimeStampField;
    FDQuery1StartDateTimeZone: TWideStringField;
    FDQuery1EndDate: TDateField;
    FDQuery1EndDateTime: TSQLTimeStampField;
    FDQuery1EndDateTimeZone: TWideStringField;
    FDQuery1OriginalStartTimeDateTime: TSQLTimeStampField;
    FDQuery1SendNotification: TBooleanField;
    FDQuery1Kind: TWideStringField;
    FDQuery1ETag: TWideStringField;
    FDQuery1Status: TWideStringField;
    FDQuery1HTML_Link: TWideStringField;
    FDQuery1Locked: TBooleanField;
    FDQuery1SourceTitle: TWideStringField;
    FDQuery1SourceURL: TWideStringField;
    FDQuery1HangoutLink: TWideStringField;
    FDQuery1Created: TSQLTimeStampField;
    FDQuery1Updated: TSQLTimeStampField;
    FDQuery1ColorId: TIntegerField;
    FDQuery1CreatorEmail: TWideStringField;
    FDQuery1CreatorDisplayName: TWideStringField;
    FDQuery1OrganizerEmail: TWideStringField;
    FDQuery1OrganizerDisplayName: TWideStringField;
    FDQuery1Recurrences: TWideStringField;
    FDQuery1RecurringEventId: TWideStringField;
    FDQuery1Transparency: TWideStringField;
    FDQuery1Visibility: TWideStringField;
    FDQuery1ICalUid: TWideStringField;
    FDQuery1Sequence: TWideStringField;
    FDQuery1AttendeesEmails: TWideStringField;
    FDQuery1AttendeesDisplayNames: TWideStringField;
    FDQuery1AttendeesResponseStatus: TWideStringField;
    FDQuery1AttendeesOmitted: TBooleanField;
    FDQuery1ExtendedPropertiesPrivateKey: TWideStringField;
    FDQuery1ExtendedPropertiesPrivateValue: TWideStringField;
    FDQuery1ExtendedPropertiesSharedKey: TWideStringField;
    FDQuery1ExtendedPropertiesSharedValue: TWideStringField;
    FDQuery1AnyoneCanAddSelf: TBooleanField;
    FDQuery1GuestsCanInviteOthers: TBooleanField;
    FDQuery1GuestsCanSeeOtherGuests: TBooleanField;
    FDQuery1GuestsCanModify: TBooleanField;
    FDQuery1PrivateCopy: TBooleanField;
    FDQuery1RemindersUseDefault: TBooleanField;
    FDQuery1ReminderOverrideMethods: TWideStringField;
    FDQuery1ReminderOverrideMinutes: TWideStringField;
    FDQuery1SearchTerms: TWideStringField;
    FDQuery1SingleEvents: TWideStringField;
    FDQuery1ConferenceId: TWideStringField;
    FDQuery1ConferenceNotes: TWideStringField;
    FDQuery1ConferenceSignature: TWideStringField;
    FDQuery1ConferenceSolutionName: TWideStringField;
    FDQuery1ConferenceSolutionKeyType: TWideStringField;
    FDQuery1ConferenceSolutionIconUri: TWideStringField;
    FDQuery1ConferenceRequestId: TWideStringField;
    FDQuery1ConferenceRequestKey: TWideStringField;
    FDQuery1ConferenceRequestKeyType: TWideStringField;
    FDQuery1ConferenceRequestStatus: TWideStringField;
    FDQuery1ConferenceRequestStatusCode: TWideStringField;
    FDQuery1ConferenceEntryPointsAggregate: TWideStringField;
    FDQuery1GadgetTitle: TWideStringField;
    FDQuery1GadgetType: TWideStringField;
    FDQuery1GadgetDisplay: TWideStringField;
    FDQuery1GadgetHeight: TIntegerField;
    FDQuery1GadgetWidth: TIntegerField;
    FDQuery1GadgetIconLink: TWideStringField;
    FDQuery1GadgetLink: TWideStringField;
    FDQuery1EventType: TWideStringField;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FDConnection1.Connected := True;
  FDQuery1.Open;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  sSQL: string;
begin
  sSQL := 'INSERT INTO "fernando.rizzato@gmail.com" ' +
    ' (Summary, Description, StartDateTime, EndDateTime) VALUES (' +
    QuotedStr('Evento Teste') + ', ' + QuotedStr('Descrição Teste') + ',' +
    QuotedStr('06/28/2022 10:00') + ',' + QuotedStr('06/28/2022 11:00') + ')';
  FDConnection1.ExecSQL(sSQL);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FDConnection1.Connected := False;
  FDQuery1.Close;
end;

end.
