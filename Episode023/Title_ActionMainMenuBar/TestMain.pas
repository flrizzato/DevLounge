unit TestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.TitleBarCtrls,
  Vcl.FormTabsBar, System.ImageList, Vcl.ImgList, Vcl.Buttons,
  Vcl.Themes, System.Actions,
  Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.ActnCtrls,
  Vcl.ActnMenus, Vcl.StdActns, Vcl.ToolWin;

type
  TfrmTest = class(TForm)
    TitleBarPanel1: TTitleBarPanel;
    Button1: TButton;
    ActionList1: TActionList;
    FileNew1: TAction;
    FileOpen1: TAction;
    FileClose1: TWindowClose;
    FileSave1: TAction;
    FileSaveAs1: TAction;
    FileExit1: TAction;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    WindowCascade1: TWindowCascade;
    WindowTileHorizontal1: TWindowTileHorizontal;
    WindowTileVertical1: TWindowTileVertical;
    WindowMinimizeAll1: TWindowMinimizeAll;
    HelpAbout1: TAction;
    ActionManager1: TActionManager;
    ActionMainMenuBar1: TActionMainMenuBar;
    procedure Button1Click(Sender: TObject);
    procedure ActionMainMenuBar1CanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure EditCut1Execute(Sender: TObject);
    procedure EditCopy1Execute(Sender: TObject);
    procedure EditPaste1Execute(Sender: TObject);
    procedure FileNew1Execute(Sender: TObject);
    procedure FileOpen1Execute(Sender: TObject);
    procedure FileClose1Execute(Sender: TObject);
    procedure FileSave1Execute(Sender: TObject);
    procedure FileSaveAs1Execute(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
    procedure HelpAbout1Execute(Sender: TObject);
    procedure WindowCascade1Execute(Sender: TObject);
    procedure WindowTileHorizontal1Execute(Sender: TObject);
    procedure WindowTileVertical1Execute(Sender: TObject);
    procedure WindowMinimizeAll1Execute(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTest: TfrmTest;

implementation

{$R *.dfm}

procedure TfrmTest.ActionMainMenuBar1CanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  NewWidth := ScaleValue(160);
end;

procedure TfrmTest.Button1Click(Sender: TObject);
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

procedure TfrmTest.EditCopy1Execute(Sender: TObject);
begin
  //
end;

procedure TfrmTest.EditCut1Execute(Sender: TObject);
begin
  //
end;

procedure TfrmTest.EditPaste1Execute(Sender: TObject);
begin
  //
end;

procedure TfrmTest.FileClose1Execute(Sender: TObject);
begin
 //
end;

procedure TfrmTest.FileExit1Execute(Sender: TObject);
begin
  //
end;

procedure TfrmTest.FileNew1Execute(Sender: TObject);
begin
  //
end;

procedure TfrmTest.FileOpen1Execute(Sender: TObject);
begin
  //
end;

procedure TfrmTest.FileSave1Execute(Sender: TObject);
begin
 //
end;

procedure TfrmTest.FileSaveAs1Execute(Sender: TObject);
begin
  //
end;

procedure TfrmTest.FormCreate(Sender: TObject);
begin
  ActionMainMenuBar1.Align := alLeft;
end;

procedure TfrmTest.FormResize(Sender: TObject);
begin
  if WindowState = wsMaximized then
    ActionMainMenuBar1.Margins.Top := 0
  else
    ActionMainMenuBar1.Margins.Top := ScaleValue(2);
end;

procedure TfrmTest.HelpAbout1Execute(Sender: TObject);
begin
  //
end;

procedure TfrmTest.WindowCascade1Execute(Sender: TObject);
begin
  //
end;

procedure TfrmTest.WindowMinimizeAll1Execute(Sender: TObject);
begin
  //
end;

procedure TfrmTest.WindowTileHorizontal1Execute(Sender: TObject);
begin
  //
end;

procedure TfrmTest.WindowTileVertical1Execute(Sender: TObject);
begin
  //
end;

end.
