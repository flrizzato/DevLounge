unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Grid,
  FMX.TabControl, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IB, FireDAC.Phys.IBDef,
  FireDAC.FMXUI.Wait, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Fmx.Bind.Grid,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components,
  Data.Bind.Grid, Data.Bind.DBScope, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FMX.Memo.Types, FMX.Memo, FMX.Edit, FMX.StdCtrls,
  FMX.Layouts, SmartCoreAI.Types, SmartCoreAI.Driver.OpenAI,
  SmartCoreAI.Comp.Connection, SmartCoreAI.Comp.Chat, System.Skia, FMX.Skia;

type
  TMainForm = class(TForm)
    TabControlMain: TTabControl;
    tabPrompt: TTabItem;
    tabSQL: TTabItem;
    stgResult: TStringGrid;
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    memPrompt: TMemo;
    memSQL: TMemo;
    Layout1: TLayout;
    butSQL: TSpeedButton;
    butPrompt: TSpeedButton;
    tabMeta: TTabItem;
    memMeta: TMemo;
    AIConnection1: TAIConnection;
    AIChatRequest1: TAIChatRequest;
    AIOpenAIDriver1: TAIOpenAIDriver;
    Splitter1: TSplitter;
    StyleBook1: TStyleBook;
    skaProgress: TSkAnimatedImage;
    butClose: TSpeedButton;
    procedure AIChatRequest1Error(Sender: TObject; const ErrorMessage: string);
    procedure AIChatRequest1Response(Sender: TObject; const Text: string);
    procedure butCloseClick(Sender: TObject);
    procedure butPromptClick(Sender: TObject);
    procedure butSQLClick(Sender: TObject);
  private
    { Private declarations }
    function EnsureApiKeyFromIni(out AApiKey: string; const ASection: string = 'OpenAI'; const AIdent: string   = 'ApiKey'; const AIniPath: string = ''): Boolean;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses System.IniFiles, System.IOUtils, FMX.DialogService.Sync;

{$R *.fmx}

procedure TMainForm.AIChatRequest1Error(Sender: TObject; const ErrorMessage:
    string);
begin
  raise Exception.Create('Error Message: ' + ErrorMessage);
  skaProgress.Animation.Enabled := False;
end;

procedure TMainForm.AIChatRequest1Response(Sender: TObject; const Text: string);
begin
  memSQL.Text := Text;
  TabControlMain.ActiveTab := tabSQL;
  skaProgress.Animation.Enabled := False;
end;

procedure TMainForm.butCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.butPromptClick(Sender: TObject);
var
  LAPIKey, LPrompt: string;

  function EnsureValidPrompt(var S: string): boolean;
   begin
     S := '';
     for var i := 0 to memPrompt.lines.Count-1 do
      if not memPrompt.Lines[i].StartsWith('--')  then
        S := S + memPrompt.Lines[i];
     Result := not S.IsEmpty;
   end;

begin
  if not EnsureValidPrompt(LPrompt) then
    raise Exception.Create('Error Message: A valid prompt is needed!');

  if not EnsureApiKeyFromIni(LAPIKey) then
    raise Exception.Create('Error Message: A valid key is needed!');

  const PromptBase =
  '''
    You are a SQL expert.
    Follow these rules:
    - The database in use is Interbase 15, consider the supported syntax.
    - In InterBase there isn’t a TOP keyword like in SQL Server,
      but you can achieve the same effect using the ROWS clause.
    - Only use tables/columns provided in the schema.
    - If user asks for info not in schema, ask for clarification.
    - Return only SQL as result, no additional comments.
    Here is the database schema:
  ''';

  TAIOpenAIParams(AIOpenAIDriver1.Params).APIKey := LAPIKey;

  var PromptFull := PromptBase + #13 +
                    memMeta.Lines.Text + #13 +
                    'Here is the question: ' + LPrompt;

  skaProgress.Animation.Enabled := True;
  AIChatRequest1.Chat(PromptFull);
end;

procedure TMainForm.butSQLClick(Sender: TObject);
begin
  FDQuery1.Open(memSQL.Text);
end;

function TMainForm.EnsureApiKeyFromIni(out AApiKey: string; const ASection,
  AIdent, AIniPath: string): Boolean;
var
  IniFileName: string;
  Ini: TIniFile;
  Tmp: string;

  function PromptForApiKey(var S: string): Boolean;
  var
    Values: TArray<string>;
  begin
    SetLength(Values, 1);
    Values[0] := S;
    Result := TDialogServiceSync.InputQuery('Please Enter the API Key', ['API Key'], Values);
    if Result and (Length(Values) > 0) then
      S := Values[0];
  end;

begin
  AApiKey := TAIOpenAIParams(AIOpenAIDriver1.Params).APIKey;
  if not AApiKey.IsEmpty then
    Exit(True);

  IniFileName := AIniPath;
  if IniFileName = '' then
    IniFileName := TPath.ChangeExtension(ParamStr(0), '.ini');

  Ini := TIniFile.Create(IniFileName);
  try
    Tmp := Trim(Ini.ReadString(ASection, AIdent, ''));
    while Tmp = '' do
    begin
      if not PromptForApiKey(Tmp) then
        Exit(False); // user cancelled
      Tmp := Trim(Tmp);
    end;
    if Ini.ReadString(ASection, AIdent, '') <> Tmp then
      Ini.WriteString(ASection, AIdent, Tmp);
    AApiKey := Tmp;
    Result := True;
  finally
    Ini.Free;
  end;
end;

end.
