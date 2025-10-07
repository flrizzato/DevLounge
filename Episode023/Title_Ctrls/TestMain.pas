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
    Button1: TButton;
    Edit1: TEdit;
    ComboBox1: TComboBox;
    CheckBox1: TCheckBox;
    Button2: TButton;
    PopupMenu1: TPopupMenu;
    Option11: TMenuItem;
    Optioin21: TMenuItem;
    Option31: TMenuItem;
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
