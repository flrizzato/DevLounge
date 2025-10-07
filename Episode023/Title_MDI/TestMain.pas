unit TestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.TitleBarCtrls,
  Vcl.FormTabsBar, System.ImageList, Vcl.ImgList, Vcl.Buttons,
  Vcl.VirtualImageList, Vcl.BaseImageCollection, Vcl.ImageCollection,
  Vcl.ComCtrls, Vcl.ToolWin, Vcl.ExtCtrls, Vcl.Themes, Vcl.Menus;

type
  TfrmTest = class(TForm)
    TitleBarPanel1: TTitleBarPanel;
    ImageCollection1: TImageCollection;
    VirtualImageList1: TVirtualImageList;
    ToolBar1: TToolBar;
    ToolButton2: TToolButton;
    FormTabsBar1: TFormTabsBar;
    ToolButton3: TToolButton;
    PopupMenu1: TPopupMenu;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Print1: TMenuItem;
    PrintSetup1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    procedure TitleBarPanel1CustomButtons0Paint(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure TitleBarPanel1CustomButtons0Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTest: TfrmTest;
  Count: Integer = 0;

implementation

{$R *.dfm}

uses
  TestChild;

procedure TfrmTest.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmTest.New1Click(Sender: TObject);
begin
  Inc(Count);
  var lTestForm := TfrmTestChild.Create(Self);
  lTestForm.Show;
  lTestForm.Caption := 'Form' + Count.ToString
end;

procedure TfrmTest.TitleBarPanel1CustomButtons0Click(Sender: TObject);
begin
  New1Click(Self);
end;

procedure TfrmTest.TitleBarPanel1CustomButtons0Paint(Sender: TObject);
begin
  var LButton := TSystemTitlebarButton(Sender);
  var R := LButton.ClientRect;
  InflateRect(R, -ScaleValue(6), -ScaleValue(6));
  ImageCollection1.Draw(LButton.Canvas, R, 3, True);
end;

procedure TfrmTest.ToolButton3Click(Sender: TObject);
begin
  if TStyleManager.ActiveStyle.Name = 'Windows' then
    TStyleManager.TrySetStyle('Aqua Light Slate')
  else
  if TStyleManager.ActiveStyle.Name = 'Aqua Light Slate' then
    TStyleManager.TrySetStyle('Tablet Dark')
  else
  if TStyleManager.ActiveStyle.Name = 'Tablet Dark' then
    TStyleManager.TrySetStyle('Windows');
end;

end.
