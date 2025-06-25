unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.UITypes, Vcl.Graphics, Vcl.StdCtrls,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Clipbrd, Data.DB,

  VCLTee.Control,
  VCLTee.Grid,
  VCLTee.Grid.Themes,
  Tee.Grid,
  Tee.Format,
  Tee.Renders,
  Tee.GridData,
  Tee.GridData.DB,
  Tee.Grid.Columns,
  Tee.Grid.RowGroup,
  Tee.Grid.CSV,
  Tee.Grid.Totals,
  Tee.Grid.Header;

type
  TLastSorted = record
  public
    Column: TColumn;
    Ascending: Boolean;
  end;

  TMainForm = class(TForm)
    TeeGrid1: TTeeGrid;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    LastSorted: TLastSorted;
    Expander: TExpanderRender;

    procedure myPaint(const Sender: TColumn; var AData: TRenderData;
      var DefaultPaint: Boolean);

    procedure GetSalaries(const Sender: TExpanderRender; const ARow: Integer;
      out AData: TObject);
    procedure DetailNewGroup(const Sender, NewGroup: TRowGroup);

    // Methods to enable custom grid Sort by mouse click at Header
    procedure CanSortBy(const AColumn: TColumn; var CanSort: Boolean);
    function CreateSortable: TSortableHeader;
    procedure SortBy(Sender: TObject; const AColumn: TColumn);
    procedure SortData(const AColumn: TColumn; const Ascending: Boolean);
    procedure SortState(const AColumn: TColumn; var State: TSortState);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses uMainDM;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  TVCLGridThemes.ApplyTo(TeeGrid1);
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  TeeGrid1.Columns[0].ParentFormat := False;
  TeeGrid1.Columns[0].Format.Font.Color := clRed;
end;

procedure TMainForm.myPaint(const Sender: TColumn; var AData: TRenderData;
  var DefaultPaint: Boolean);
begin
  DefaultPaint := True;
  if AData.Data.ToDouble <= 27000 then
  begin
    AData.Painter.SetFontColor(clRed);
    AData.Painter.Fill(AData.Bounds, clYellow);
  end
  else
    AData.Painter.SetFontColor(clBlack);
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  TeeGrid1.Columns['SALARY'].OnPaint := myPaint;
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
  TeeGrid1.Columns['EMP_NO'].Locked := TColumnLocked.Left;
  TeeGrid1.Columns['EMP_NO'].ParentFormat := False;
  TeeGrid1.Columns['EMP_NO'].Format.Brush.Show;
  TeeGrid1.Columns['EMP_NO'].Format.Brush.Color := TColors.Navajowhite;
end;

procedure TMainForm.Button5Click(Sender: TObject);
begin
  TeeGrid1.Rows.Height.Automatic := False;
  TeeGrid1.Rows.Height.Value := 50;
end;

procedure TMainForm.Button6Click(Sender: TObject);
begin
  // Create "Expander"
  Expander := TeeGrid1.Grid.Current.NewExpander;

  // Setup event
  Expander.OnGetData := GetSalaries;
  TeeGrid1.Grid.Current.OnNewDetail := DetailNewGroup;

  // We don't know in advance if a row can be expanded or not, so set Always
  Expander.AlwaysExpand := True;

  // Set to first Column
  if TeeGrid1.Columns.Count > 0 then
    TeeGrid1.Columns[0].Render := Expander;
end;

procedure TMainForm.Button7Click(Sender: TObject);
begin
  TeeGrid1.Selected.Range.Enabled := True;
end;

procedure TMainForm.Button8Click(Sender: TObject);
begin
  Clipboard.AsText := TCSVData.From(TeeGrid1.Grid, TeeGrid1.Selected);
end;

procedure TMainForm.Button9Click(Sender: TObject);
var
  Totals: TColumnTotals;
begin
  Totals := TColumnTotals.Create(TeeGrid1.Footer);
  Totals.Calculation.Add('Salary', TColumnCalculation.Sum);
  Totals.Format.Brush.Gradient.Visible := False;
  Totals.Format.Brush.Gradient.Visible := False;
  Totals.Format.Brush.Color := clYellow;
  Totals.Format.Brush.Visible := True;
  Totals.Format.Font.Size := 9;
  Totals.Format.Font.Style := [fsBold];
end;

procedure TMainForm.GetSalaries(const Sender: TExpanderRender;
  const ARow: Integer; out AData: TObject);
begin
  // Return a new Data using a clone of Orders rows for a given Customer
  AData := TVirtualDBData.From(MainDM.SalaryOfEmployee(ARow + 1));

  // Data should be destroyed automatically
  TVirtualDBData(AData).OwnsData := True;
end;

procedure TMainForm.DetailNewGroup(const Sender, NewGroup: TRowGroup);
begin
  NewGroup.Header.Format.Brush.Gradient.Visible := False;
  NewGroup.Header.Format.Brush.Color := clYellow;
  NewGroup.Header.Format.Brush.Visible := True;
  NewGroup.Header.Format.Font.Size := 9;
  NewGroup.Cells.Format.Font.Size := 9;

  for var i := 0 to NewGroup.Columns.Count - 1 do
    NewGroup.Columns[i].Width.Value := 155;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Set Header sortable
  TeeGrid1.Header.Sortable := True;
  TeeGrid1.Header.SortRender := CreateSortable;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  MainDM.EmployeeTable.Open;
end;

procedure TMainForm.SortBy(Sender: TObject; const AColumn: TColumn);
var
  tmp: TSortState;
begin
  // Get current sort order for AColumn
  SortState(AColumn, tmp);

  // Invert order ( Ascending <--> Descending )
  LastSorted.Ascending := tmp <> TSortState.Ascending;

  // Sort data
  SortData(AColumn, LastSorted.Ascending);

  // Remember last sorted column
  LastSorted.Column := AColumn;
end;

procedure TMainForm.SortData(const AColumn: TColumn; const Ascending: Boolean);
begin
  var
    AscDsc: string := 'A';
  if not Ascending then
    AscDsc := 'D';
  MainDM.EmployeeTable.DisableControls;
  try
    MainDM.EmployeeTable.IndexFieldNames := AColumn.Header.Text + ':' + AscDsc;
    MainDM.EmployeeTable.First;
  finally
    MainDM.EmployeeTable.EnableControls;
  end;
end;

procedure TMainForm.SortState(const AColumn: TColumn; var State: TSortState);
begin
  if AColumn = LastSorted.Column then
    if LastSorted.Ascending then
      State := TSortState.Ascending
    else
      State := TSortState.Descending
  else
    State := TSortState.None;
end;

procedure TMainForm.CanSortBy(const AColumn: TColumn; var CanSort: Boolean);
begin
  CanSort := (AColumn <> nil);
end;

function TMainForm.CreateSortable: TSortableHeader;
begin
  result := TSortableHeader.Create(TeeGrid1.Header.Changed);

  // cosmetic example
  result.Format.Brush.Color := TColors.Red;
  result.Format.Stroke.Show;

  // Set custom events
  result.OnCanSort := CanSortBy;
  result.OnSortBy := SortBy;
  result.OnSortState := SortState;
end;

end.
