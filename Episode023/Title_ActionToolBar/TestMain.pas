unit TestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.TitleBarCtrls,
  Vcl.FormTabsBar, System.ImageList, Vcl.ImgList, Vcl.Buttons,
  Vcl.Themes, System.Actions,
  Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.ActnCtrls,
  Vcl.ActnMenus, Vcl.StdActns, Vcl.ToolWin, Vcl.DBActns,
  Vcl.BaseImageCollection, Vcl.ImageCollection, Vcl.VirtualImageList,
  Vcl.ExtCtrls;

type
  TfrmTest = class(TForm)
    TitleBarPanel1: TTitleBarPanel;
    Button1: TButton;
    ActionToolBar1: TActionToolBar;
    ImageCollection1: TImageCollection;
    ActionManager1: TActionManager;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    VirtualImageList1: TVirtualImageList;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTest: TfrmTest;

implementation

{$R *.dfm}

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

end.
