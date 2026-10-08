{
  This unit defines the model/repository for the Tasks.
  It implements a singleton pattern to manage a list of tasks.
}

unit Models.Tasks;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  System.SyncObjs;

type
  TTaskItem = class
  private
    FId: Integer;
    FDescription: string;
    FCompleted: Boolean;
  public
    constructor Create(AId: Integer; const ADescription: string);
    property Id: Integer read FId;
    property Description: string read FDescription write FDescription;
    property Completed: Boolean read FCompleted write FCompleted;
  end;

  // In this demo the Tasks are stored in memory and to avoid issues in
  // multi-thread environments this class is instantiated as a singleton
  TTasks = class
  private
    class var FInstance: TTasks;
    class var FLock: TCriticalSection;

    FItems: TObjectList<TTaskItem>;
    FNextId: Integer;

    constructor Create;
    function GetCount: Integer;
    function GetCompletedCount: Integer;
    function GetAllTasks: TObjectList<TTaskItem>;
    function GetNextId: Integer;
  public
    class constructor ClassCreate;
    class destructor ClassDestroy;
    class function GetInstance: TTasks;

    destructor Destroy; override;
    function FindTaskById(AId: Integer): TTaskItem;
    function AddTask(const ADescription: string): TTaskItem;
    procedure EditTask(AId: Integer; ADescription: string);
    procedure DeleteTask(AId: Integer);
    function ToggleCompletedTask(AId: Integer): TTaskItem;
    property Count: Integer read GetCount;
    property CompletedCount: Integer read GetCompletedCount;
    property NextId: Integer read GetNextId;
    property AllTasks: TObjectList<TTaskItem> read GetAllTasks;
  end;

implementation

{ TTaskItem }

constructor TTaskItem.Create(AId: Integer; const ADescription: string);
begin
  inherited Create;
  FId := AId;
  FDescription := ADescription;
  FCompleted := False;
end;

{ TTasks }

class constructor TTasks.ClassCreate;
begin
  FLock := TCriticalSection.Create;
end;

class destructor TTasks.ClassDestroy;
begin
  FInstance.Free;
  FLock.Free;
end;

class function TTasks.GetInstance: TTasks;
begin
  if not Assigned(FInstance) then
  begin
    FLock.Acquire;
    try
      if not Assigned(FInstance) then
        begin
          FInstance := TTasks.Create;
          FInstance.AddTask('Refactor that spaghetti code written in the 90s.');
          FInstance.AddTask('Convince management to upgrade from Delphi 7.');
          FInstance.AddTask('Remove all the captions of the TPanels.');
          FInstance.AddTask('Use `with` statement responsibly... and then regret it immediately.');
          FInstance.AddTask('Refactor all the business logic inside OnClick events.');
          FInstance.AddTask('Convince the team that VCL is still cool.');
          FInstance.AddTask('Document those "temporary" global variables created 5 years ago.');
          FInstance.TogglecompletedTask(2);
        end;
    finally
      FLock.Release;
    end;
  end;
  Result := FInstance;
end;

constructor TTasks.Create;
begin
  inherited;
  FItems := TObjectList<TTaskItem>.Create(True);
  FNextId := 1;
end;

destructor TTasks.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TTasks.GetNextId: Integer;
begin
  Result := FNextId;
end;

procedure TTasks.EditTask(AId: Integer; ADescription: string);
var
  Item: TTaskItem;
begin
  Item := FindTaskById(AId);
  if Assigned(Item) then
    Item.Description := ADescription;
end;

function TTasks.AddTask(const ADescription: string): TTaskItem;
var
  NewItem: TTaskItem;
begin
  NewItem := TTaskItem.Create(FNextId, ADescription);
  FItems.Add(NewItem);
  Result := NewItem;
  Inc(FNextId);
end;

function TTasks.ToggleCompletedTask(AId: Integer): TTaskItem;
var
  Item: TTaskItem;
begin
  Item := FindTaskById(AId);
  if Assigned(Item) then
  begin
    Item.Completed := not(Item.Completed);
    Exit(Item);
  end;
  Result := nil;
end;

procedure TTasks.DeleteTask(AId: Integer);
var
  Item: TTaskItem;
begin
  Item := FindTaskById(AId);
  if Assigned(Item) then
    FItems.Remove(Item);
end;

function TTasks.GetCompletedCount: Integer;
var
  Item: TTaskItem;
begin
  Result := 0;
  for Item in FItems do
    if Item.Completed then
      Inc(Result);
end;

function TTasks.GetAllTasks: TObjectList<TTaskItem>;
begin
  Result := FItems;
end;

function TTasks.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TTasks.FindTaskById(AId: Integer): TTaskItem;
var
  Item: TTaskItem;
begin
  for Item in FItems do
    if Item.Id = AId then
      Exit(Item);
  Result := nil;
end;

end.
