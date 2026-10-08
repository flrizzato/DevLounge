{
  This unit defines the model/repository for the Tasks.
  It implements a singleton pattern to manage a list of tasks.
}

unit Models.Tasks;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  System.SyncObjs,
  System.Classes,
  System.Variants,
  FireDAC.Comp.Client;

type
  TTaskItem = class(TPersistent)
  private
    FId: Integer;
    FDescription: string;
    FCompleted: Boolean;
  public
    constructor Create(AId: Integer; const ADescription: string; const Acompleted: boolean = false);
    property Id: Integer read FId;
     property Description: string read FDescription write FDescription;
    property Completed: Boolean read FCompleted write FCompleted;
  published
    property TaskId: Integer read FId;
     property TaskDescription: string read FDescription;
    property IsCompleted: Boolean read FCompleted;
  end;

  TTasks = class(TObject)
  private
    FDConnection: TFDConnection;

    function GetCount: Integer;
    function GetCompletedCount: Integer;
    function GetAllTasks: TObjectList<TTaskItem>;
  public
    constructor Create(AFDConnection: TFDConnection);
    function FindTaskById(AId: Integer): TTaskItem;
    procedure AddTask(const ADescription: string);
    procedure EditTask(AId: Integer; ADescription: string);
    procedure DeleteTask(AId: Integer);
    procedure ToggleCompletedTask(AId: Integer);
  published
    property Count: Integer read GetCount;
    property CompletedCount: Integer read GetCompletedCount;
    property AllTasks: TObjectList<TTaskItem> read GetAllTasks;
  end;

implementation

uses Data.DB;

{ TTaskItem }

constructor TTaskItem.Create(AId: Integer; const ADescription: string; const Acompleted: boolean = false);
begin
  inherited Create;
  FId := AId;
  FDescription := ADescription;
  FCompleted := Acompleted;
end;

{ TTasks }

constructor TTasks.Create(AFDConnection: TFDConnection);
begin
  inherited Create;
  FDConnection := AFDConnection;
end;

procedure TTasks.EditTask(AId: Integer; ADescription: string);
begin
  FDConnection.ExecSQL('UPDATE tasks SET description = :description WHERE id = :id', [ADescription, AId]);
end;

procedure TTasks.AddTask(const ADescription: string);
var
  LQuery: TFDQuery;
  LNextId: Integer;
begin
  LQuery := TFDQuery.Create(nil);
  try
    LQuery.Connection := FDConnection;
    LQuery.SQL.Text := 'SELECT MAX(id) FROM tasks';
    LQuery.Open;
    if (LQuery.FieldCount > 0) and not(LQuery.Fields[0].IsNull) then
      LNextId := LQuery.Fields[0].AsInteger + 1
    else
      LNextId := 1;
    LQuery.Close;
  finally
    LQuery.Free;
  end;

  FDConnection.ExecSQL('INSERT INTO tasks(id, description, completed) VALUES (:id, :description, :completed)',
    [LNextId, ADescription, False]);
end;

procedure TTasks.ToggleCompletedTask(AId: Integer);
begin
  FDConnection.ExecSQL('EXECUTE PROCEDURE toggle_completed_task :id', [AId]);
end;

procedure TTasks.DeleteTask(AId: Integer);
begin
  FDConnection.ExecSQL('DELETE FROM tasks WHERE id = :1', [AId]);
end;

function TTasks.GetCompletedCount: Integer;
begin
  Result := FDConnection.ExecSQLScalar('SELECT COUNT(*) FROM tasks WHERE completed = True');
  if VarIsNull(Result) then Result := 0;
end;

function TTasks.GetAllTasks: TObjectList<TTaskItem>;
var
  LItems: TObjectList<TTaskItem>;
  LQuery: TFDQuery;
begin
  LItems := TObjectList<TTaskItem>.Create(True);
  LQuery := TFDQuery.Create(nil);
  try
    LQuery.Connection := FDConnection;
    LQuery.SQL.Text := 'SELECT * FROM tasks ORDER BY id';
    LQuery.Open;
    while not LQuery.Eof do
    begin
      LItems.Add(TTaskItem.Create( LQuery.FieldByName('id').AsInteger,
                                   LQuery.FieldByName('description').AsString,
                                   LQuery.FieldByName('completed').AsBoolean));
      LQuery.Next;
    end;
    Result := LItems;
    LQuery.Close;
  finally
    LQuery.Free;
  end;
end;

function TTasks.GetCount: Integer;
begin
  Result := FDConnection.ExecSQLScalar('SELECT COUNT(*) FROM tasks');
  if VarIsNull(Result) then Result := 0;
end;

function TTasks.FindTaskById(AId: Integer): TTaskItem;
var
  LQuery: TFDQuery;
begin
  Result := nil;
  LQuery := TFDQuery.Create(nil);
  try
    LQuery.Connection := FDConnection;
    LQuery.SQL.Text := 'SELECT * FROM tasks WHERE id = :id';
    LQuery.ParamByName('id').AsInteger := AId;
    LQuery.Open;
    if not LQuery.Eof then
    begin
      Result := TTaskItem.Create( LQuery.FieldByName('id').AsInteger,
                                  LQuery.FieldByName('description').AsString,
                                  LQuery.FieldByName('completed').AsBoolean);
    end;
    LQuery.Close;
  finally
    LQuery.Free;
  end;
end;

end.
