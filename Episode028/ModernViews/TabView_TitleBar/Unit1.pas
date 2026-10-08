unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.UITypes,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Themes, Vcl.WinXPanels, Vcl.ExtCtrls, Vcl.TabView,
  Vcl.TitleBarCtrls, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList,
  Vcl.BaseImageCollection, Vcl.ImageCollection;

type
  { AddTab cannot carry data into the tab it creates, so values for the next
    tab are queued here and consumed by OnInitTab. }
  TTabSeed = record
    Active: Boolean;
    Caption: string;
    Body: string;
    ImageIndex: Integer;
  end;

  TfrmTabViewTitleBar = class(TForm)
    TitleBarPanel1: TTitleBarPanel;
    TabView1: TTabView;
    CardPanel1: TCardPanel;
    pnlFooter: TPanel;
    lblStyle: TLabel;
    cmbStyle: TComboBox;
    ImageCollection1: TImageCollection;
    VirtualImageList1: TVirtualImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmbStyleChange(Sender: TObject);
    procedure TabView1ChangeTab(Sender: TObject);
    procedure TabView1CloseTab(Sender: TObject; ATab: TTabViewTab; var ACanClose: Boolean);
    procedure TabView1InitTab(Sender: TObject; ATab: TTabViewTab);
  private
    FTabCount: Integer;
    FSeed: TTabSeed;
    procedure FillStyleList;
    procedure ApplyTitleBarColors;
    procedure SeedInitialTabs;
    function CreatePageContent(const ATitle, ABody: string): TCard;
  public
  end;

var
  frmTabViewTitleBar: TfrmTabViewTitleBar;

implementation

{$R *.dfm}

procedure TfrmTabViewTitleBar.FillStyleList;
begin
  cmbStyle.Items.BeginUpdate;
  try
    cmbStyle.Items.Clear;
    for var StyleName in TStyleManager.StyleNames do
      cmbStyle.Items.Add(StyleName);
    cmbStyle.ItemIndex := cmbStyle.Items.IndexOf(TStyleManager.ActiveStyle.Name);
  finally
    cmbStyle.Items.EndUpdate;
  end;
end;

procedure TfrmTabViewTitleBar.ApplyTitleBarColors;
var
  LWindow, LBtnText, LBtnFace: TColor;
begin
  LWindow := StyleServices(Self).GetSystemColor(clWindow);
  LBtnText := StyleServices(Self).GetSystemColor(clBtnText);
  LBtnFace := StyleServices(Self).GetSystemColor(clBtnFace);
  CustomTitleBar.SystemColors := False;
  CustomTitleBar.StyleColors := True;
  CustomTitleBar.BackgroundColor := LWindow;
  CustomTitleBar.InactiveBackgroundColor := LWindow;
  CustomTitleBar.ForegroundColor := LBtnText;
  CustomTitleBar.InactiveForegroundColor := clGrayText;
  CustomTitleBar.ButtonForegroundColor := LBtnText;
  CustomTitleBar.ButtonBackgroundColor := LWindow;
  CustomTitleBar.ButtonInactiveForegroundColor := clGrayText;
  CustomTitleBar.ButtonInactiveBackgroundColor := LWindow;
  CustomTitleBar.ButtonHoverBackgroundColor := LBtnFace;
  CustomTitleBar.ButtonHoverForegroundColor := LBtnText;
  CustomTitleBar.ButtonPressedBackgroundColor := StyleServices(Self).GetSystemColor(clBtnShadow);
  CustomTitleBar.ButtonPressedForegroundColor := LBtnText;
end;

function TfrmTabViewTitleBar.CreatePageContent(const ATitle, ABody: string): TCard;
var
  LTitle, LBody: TLabel;
begin
  Result := CardPanel1.CreateNewCard;
  LTitle := TLabel.Create(Result);
  LTitle.Parent := Result;
  LTitle.SetBounds(ScaleValue(24), ScaleValue(20), ScaleValue(400), ScaleValue(36));
  LTitle.Caption := ATitle;
  LTitle.Font.Name := 'Segoe UI Semibold';
  LTitle.Font.Height := -27;
  LTitle.ParentFont := False;

  LBody := TLabel.Create(Result);
  LBody.Parent := Result;
  LBody.SetBounds(ScaleValue(24), ScaleValue(68), ScaleValue(520), ScaleValue(90));
  LBody.Caption := ABody;
  LBody.WordWrap := True;
  LBody.Font.Name := 'Segoe UI';
  LBody.Font.Height := -13;
  LBody.ParentFont := False;
end;

procedure TfrmTabViewTitleBar.SeedInitialTabs;
const
  Titles: array[0..3] of string = ('Home', 'Docs', 'Preview', 'Settings');
  Bodies: array[0..3] of string = (
    'TTabView sits on TTitleBarPanel for a browser-style caption bar.',
    'Transparent + BackgroundHitTestTransparent let you drag the window from empty title-bar space.',
    'Close, add, reorder, and use the tabs menu — the same APIs as a client-area TabView.',
    'Change the VCL style below; title-bar colors follow the active style.');
begin
  for var I := 0 to High(Titles) do
  begin
    var ImgCount := VirtualImageList1.Count;
    if ImgCount < 1 then
      ImgCount := 1;
    FSeed.Active := True;
    FSeed.Caption := Titles[I];
    FSeed.Body := Bodies[I];
    FSeed.ImageIndex := I mod ImgCount;
    try
      TabView1.AddTab;
    finally
      FSeed.Active := False;
    end;
  end;
  if TabView1.Tabs.Count > 0 then
  begin
    TabView1.TabIndex := 0;
    if TabView1.Tabs[0].Control <> nil then
      CardPanel1.ActiveCard := TCard(TabView1.Tabs[0].Control);
  end;
end;

procedure TfrmTabViewTitleBar.FormCreate(Sender: TObject);
begin
  FTabCount := 0;
  FillStyleList;
  ApplyTitleBarColors;
end;

procedure TfrmTabViewTitleBar.FormShow(Sender: TObject);
begin
  if TabView1.Tabs.Count = 0 then
    SeedInitialTabs;
end;

procedure TfrmTabViewTitleBar.cmbStyleChange(Sender: TObject);
begin
  if cmbStyle.ItemIndex >= 0 then
  begin
    TStyleManager.SetStyle(cmbStyle.Items[cmbStyle.ItemIndex]);
    ApplyTitleBarColors;
  end;
end;

procedure TfrmTabViewTitleBar.TabView1ChangeTab(Sender: TObject);
begin
  if (TabView1.ActiveTab <> nil) and (TabView1.ActiveTab.Control <> nil) then
    CardPanel1.ActiveCard := TCard(TabView1.ActiveTab.Control);
end;

procedure TfrmTabViewTitleBar.TabView1CloseTab(Sender: TObject; ATab: TTabViewTab;
  var ACanClose: Boolean);
begin
  if MessageDlg('Close this tab?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    ACanClose := True;
    FreeAndNil(ATab.Control);
  end
  else
    ACanClose := False;
end;

procedure TfrmTabViewTitleBar.TabView1InitTab(Sender: TObject; ATab: TTabViewTab);
var
  LCaption, LBody: string;
  LImage: Integer;
begin
  Inc(FTabCount);
  if FSeed.Active then
  begin
    LCaption := FSeed.Caption;
    LBody := FSeed.Body;
    LImage := FSeed.ImageIndex;
  end
  else
  begin
    { Added by the "+" button. A tab created at runtime starts with an empty
      Caption - the "TabViewTabN" default only applies at design time. }
    LCaption := 'Tab ' + FTabCount.ToString;
    LBody := 'Title-bar tab content created in OnInitTab and shown in OnChangeTab.';
    var ImgCount := VirtualImageList1.Count;
    if ImgCount < 1 then
      ImgCount := 1;
    LImage := (FTabCount - 1) mod ImgCount;
  end;

  ATab.Tag := FTabCount;
  ATab.Caption := LCaption;
  ATab.ImageIndex := LImage;
  ATab.Hint := LCaption;
  ATab.Control := CreatePageContent(LCaption, LBody);
end;

end.
