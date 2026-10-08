unit Controllers.Customers;

interface

uses
  System.SysUtils,
  System.IOutils,
  FireDAC.Comp.Client,
  EMS.ResourceAPI,
  Web.Stencils,

  Helpers.FDQuery,
  Models.PaginationParams;

type
  // Simple record to hold pagination info for the template
  // TPagPageInfo = record
  //   PageNumber: Integer;
  //   PageSize: Integer;
  //   TotalPages: Integer;
  //   // Add other fields if needed by the template, e.g., TotalRecords
  // end;

  TCustomersController = class
  private
    FCustomers: TFDQuery;
    FWebStencilsProcessor: TWebStencilsProcessor;
    FWebStencilsEngine: TWebStencilsEngine;
    // Updated RenderTemplate to use TPaginationParams
    function RenderTemplate(ATemplate: string; APaginationParams: TPaginationParams): string; overload;
    function RenderTemplate(ATemplate: string): string; overload; // Overload for no pagination info
  public
    // Updated method signatures for RAD Server
    procedure GetCustomers(ARequest: TEndpointRequest; AResponse: TEndpointResponse);
    procedure GetAllCustomers(ARequest: TEndpointRequest; AResponse: TEndpointResponse);

    constructor Create(AWebStencilsEngine: TWebStencilsEngine; ACustomers: TFDQuery);
    destructor Destroy; override;
  end;

implementation

uses System.JSON; // Added for TryGetValue

// Overload for templates without pagination data
function TCustomersController.RenderTemplate(ATemplate: string): string;
begin
  FWebStencilsProcessor.InputFileName := TPath.Combine(FWebStencilsEngine.rootDirectory, 'customers/' + ATemplate + '.html');
  Result := FWebStencilsProcessor.Content;
end;

// Overload for templates with pagination data - Updated parameter type
function TCustomersController.RenderTemplate(ATemplate: string; APaginationParams: TPaginationParams): string;
begin
  FWebStencilsProcessor.InputFileName := TPath.Combine(FWebStencilsEngine.rootDirectory, 'customers/' + ATemplate + '.html');
  FWebStencilsProcessor.AddVar('customersPagination', APaginationParams, False); // Pass the class instance
  Result := FWebStencilsProcessor.Content;
  FWebStencilsProcessor.DataVars.Remove('customersPagination');
end;

constructor TCustomersController.Create(AWebStencilsEngine: TWebStencilsEngine; ACustomers: TFDQuery);
begin
  inherited Create;
  try
    FWebStencilsEngine := AWebStencilsEngine;
    FWebStencilsProcessor := TWebStencilsProcessor.Create(nil);
    FWebStencilsProcessor.Engine := FWebStencilsEngine;
    FCustomers := ACustomers;
  except
    on E: Exception do
      WriteLn('TCustomersController.Create: ' + E.Message);
  end;
end;

destructor TCustomersController.Destroy;
begin
  FWebStencilsProcessor.Free;
  inherited;
end;

procedure TCustomersController.GetCustomers(ARequest: TEndpointRequest; AResponse: TEndpointResponse);
var
  // LPageNumber, LPageSize: Integer; <-- Removed
  // LPageInfo: TPagPageInfo; <-- Removed
  // LValue: String; <-- Removed
  LPaginationParams: TPaginationParams; // <-- Use the class
begin
  LPaginationParams := nil; // Initialize
  try
    // Create pagination params object from request
    LPaginationParams := TPaginationParams.Create(ARequest, 'pagination');

    // Apply pagination settings to the query
    FCustomers.PageSize := LPaginationParams.PageSize;
    FCustomers.PageNumber := LPaginationParams.PageNumber;
    FCustomers.ApplyPagination; // This activates/refreshes the query

    // Update the total pages in the params object
    LPaginationParams.TotalPages := FCustomers.TotalPages;

    // Render template and set RAD Server response
    AResponse.Body.SetString(RenderTemplate('pagination', LPaginationParams));
  except
    on E: Exception do
      AResponse.RaiseError(500, 'Error retrieving customers', E.Message);
  end;
  LPaginationParams.Free; // Ensure the object is freed
end;

procedure TCustomersController.GetAllCustomers(ARequest: TEndpointRequest; AResponse: TEndpointResponse);
begin
  try
    FCustomers.CancelPagination; // This deactivates/refreshes the query
    // Render template and set RAD Server response
    AResponse.Body.SetString(RenderTemplate('bigtable'));
  except
    on E: Exception do
      AResponse.RaiseError(500, 'Error retrieving all customers', E.Message);
  end;
end;

end.