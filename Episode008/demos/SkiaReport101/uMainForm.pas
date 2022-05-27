unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  Skia;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure CanvasReportToPDF(fReportFile: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses FMX.Platform, System.IOUtils, Sample.Form.PDF.Viewer;

function GetOutputPath: string;
begin
{$IF DEFINED(MACOS) and NOT DEFINED(IOS)}
  Result := TPath.GetTempPath;
{$ELSEIF DEFINED(IOS) or DEFINED(ANDROID)}
  Result := TPath.GetDocumentsPath;
{$ELSE}
  Result := ExtractFilePath(ParamStr(0));
{$ENDIF}
  if (Result <> '') and not Result.EndsWith(PathDelim) then
    Result := Result + PathDelim;
end;

function getScreenWidth: Single;
var
  ScreenSvc: IFMXScreenService;
  ScreenSize: TSize;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService,
    IInterface(ScreenSvc)) then
    Result := ScreenSvc.GetScreenSize.X
  else
    Result := 200;
end;

function getScreenHeigth: Single;
var
  ScreenSvc: IFMXScreenService;
  ScreenSize: TSize;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService,
    IInterface(ScreenSvc)) then
    Result := ScreenSvc.GetScreenSize.Y
  else
    Result := 200;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  var LOutputFileName := GetOutputPath + 'report101.pdf';
  CanvasReportToPDF(LOutputFileName);
  frmPDFViewer.Show(LOutputFileName);
end;

procedure TForm1.CanvasReportToPDF(fReportFile: string);
var
  LStream: TMemoryStream;
  LDocument: ISkDocument;
  LCanvas: ISkCanvas;

  LFont: ISkFont;
  LPaint: ISkPaint;
  LTypeface: ISkTypeface;
begin
  LStream := TMemoryStream.Create;
  try
    LDocument := TSkDocument.MakePDF(LStream);
    try
      // pagina #1
      LCanvas := LDocument.BeginPage(getScreenWidth, getScreenHeigth);

      LTypeface := TSkTypeface.MakeFromName('Monospace', TSkFontStyle.Normal);
      LFont := TSkFont.Create(LTypeface, 24);
      LPaint := TSkPaint.Create(TSkPaintStyle.Fill);
      LPaint.AntiAlias := True;
      LPaint.Color := TAlphaColors.Red;
      LCanvas.DrawSimpleText('Hello from Skia PDF!', 20, 30, LFont, LPaint);

      LFont.Size := 12;
      LPaint.Color := TAlphaColors.Black;
      LCanvas.DrawSimpleText('Lorem ipsum dolor sit amet, consectetur adipiscing elit.', 20, 60, LFont, LPaint);
      LCanvas.DrawSimpleText('Aliquam condimentum nulla et lectus laoreet dapibus.', 20, 90, LFont, LPaint);
      LCanvas.DrawSimpleText('Morbi at ligula nec mauris viverra aliquam id vel ipsum.', 20, 120, LFont, LPaint);
      LCanvas.DrawSimpleText('Maecenas sed erat at quam tempor elementum quis a dolor.', 20, 150, LFont, LPaint);
      LCanvas.DrawSimpleText('Sed in metus dictum, viverra ex eget, mollis nisi.', 20, 180, LFont, LPaint);
      LCanvas.DrawSimpleText('In pellentesque purus eu arcu imperdiet malesuada.', 20, 210, LFont, LPaint);
      LCanvas.DrawSimpleText('Donec a metus ac nisi imperdiet placerat.', 20, 240, LFont, LPaint);
      LCanvas.DrawSimpleText('Mauris quis elit sed elit aliquet blandit quis sed ante.', 20, 270, LFont, LPaint);

      LDocument.EndPage;

      // pagina #2
      LCanvas := LDocument.BeginPage(getScreenWidth, getScreenHeigth);

      LPaint.setARGB($FF, $FF, 0, 0);
      LCanvas.drawLine(10, 10, 90, 10, LPaint);
      LPaint.setStrokeWidth(1);
      LPaint.setARGB($FF, 0, $FF, 0);
      LCanvas.drawLine(10, 20, 90, 20, LPaint);
      LPaint.setStrokeWidth(2);
      LPaint.setARGB($FF, 0, 0, $FF);
      LCanvas.drawLine(10, 30, 90, 30, LPaint);

      LDocument.EndPage;

    finally
      LDocument.Close;
    end;
    LStream.SaveToFile(fReportFile);
  finally
    LStream.Free;
  end;
end;

end.
