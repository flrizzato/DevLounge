unit Models.PaginationParams;

interface

uses
  System.SysUtils,
  // Web.HTTPApp; <-- Removed
  EMS.ResourceAPI; // <-- Added

type
  TPaginationParams = class(TObject) // Explicitly inherit from TObject
  private
    FPageSize: Integer;
    FPageNumber: Integer;
    FTotalPages: Integer;
    FUri: string;
    procedure ParsePaginationParams(ARequest: TEndpointRequest);
    const
      DEFAULT_PAGE_SIZE = 10;
      DEFAULT_PAGE_NUMBER = 1;
      MAX_PAGE_SIZE = 100; // Prevent excessive page sizes
  public
    constructor Create(ARequest: TEndpointRequest; AUri: String);
    property PageSize: integer read FPageSize;
    property PageNumber: integer read FPageNumber;
    property TotalPages: integer read FTotalPages write FTotalPages; // <-- Made writable
    property Uri: string read FUri;
  end;

implementation

uses System.JSON; // Added for TryGetValue

{ TPaginationParams }

constructor TPaginationParams.Create(ARequest: TEndpointRequest; AUri: String);
begin
  inherited Create; // Call inherited constructor
  FUri := AUri;
  ParsePaginationParams(ARequest);
end;

procedure TPaginationParams.ParsePaginationParams(ARequest: TEndpointRequest);
var
  LPageSizeStr, LPageNumberStr: string;
begin
  // Initialize with default values
  FPageSize := DEFAULT_PAGE_SIZE;
  FPageNumber := DEFAULT_PAGE_NUMBER;

  // Try to get PageSize parameter from RAD Server request params
  if ARequest.Params.TryGetValue('pageSize', LPageSizeStr) and (LPageSizeStr <> '') then
  begin
    FPageSize := StrToIntDef(LPageSizeStr, DEFAULT_PAGE_SIZE);
    // Validate PageSize
    if FPageSize <= 0 then
      FPageSize := DEFAULT_PAGE_SIZE
    else if FPageSize > MAX_PAGE_SIZE then
      FPageSize := MAX_PAGE_SIZE;
   end;

  // Try to get PageNumber parameter from RAD Server request params
  if ARequest.Params.TryGetValue('page', LPageNumberStr) and (LPageNumberStr <> '') then
  begin
    FPageNumber := StrToIntDef(LPageNumberStr, DEFAULT_PAGE_NUMBER);
    // Validate PageNumber
    if FPageNumber <= 0 then
      FPageNumber := DEFAULT_PAGE_NUMBER;
  end;
end;

end. 