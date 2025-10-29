unit PersonClass;

interface

uses
  System.SysUtils, System.Types;

type
  TPerson = class
  private
    FName: string;
    FAge: Integer;
    procedure SetName(const Value: string);
    procedure SetAge(const Value: Integer);
    function GetAgeBelowTen: Boolean;
  public
    constructor Create(AName: string; AAge: Integer);
    property Name: string read FName write SetName;
    property Age: Integer read FAge write SetAge;
    property AgeBelowTen: Boolean read GetAgeBelowTen;
  end;

implementation

{ TPersonObject }

constructor TPerson.Create(AName: string; AAge: Integer);
begin
  inherited Create;
  FName := AName;
  FAge := AAge;
end;

function TPerson.GetAgeBelowTen: Boolean;
begin
  Result := FAge < 10;
end;

procedure TPerson.SetAge(const Value: Integer);
begin
  FAge := Value;
end;

procedure TPerson.SetName(const Value: string);
begin
  FName := Value;
end;

end.
