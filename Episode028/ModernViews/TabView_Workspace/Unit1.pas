unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.UITypes,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Themes, Vcl.WinXPanels, Vcl.ExtCtrls, Vcl.Menus, Vcl.TabView,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList, Vcl.BaseImageCollection,
  Vcl.ImageCollection;

type
  { AddTab gives you no way to pass data into the tab it creates - it adds the
    tab, fires OnInitTab, then activates it. So the values for the next tab are
    queued here and consumed by OnInitTab. This keeps every tab built exactly
    once, instead of letting OnInitTab build a default page and then throwing
    it away. }
  TTabSeed = record
    Active: Boolean;
    Caption: string;
    Body: string;
    ImageIndex: Integer;
    Modified: Boolean;
  end;

  TfrmTabViewWorkspace = class(TForm)
    TabView1: TTabView;
    CardPanel1: TCardPanel;
    pnlOptions: TPanel;
    lblStyle: TLabel;
    cmbStyle: TComboBox;
    chkRTL: TCheckBox;
    chkDraggable: TCheckBox;
    chkHints: TCheckBox;
    chkMenuButton: TCheckBox;
    chkCloseButton: TCheckBox;
    btnLoadMany: TButton;
    lblTip: TLabel;
    ImageCollection1: TImageCollection;
    VirtualImageList1: TVirtualImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmbStyleChange(Sender: TObject);
    procedure TabView1ChangeTab(Sender: TObject);
    procedure TabView1CloseTab(Sender: TObject; ATab: TTabViewTab; var ACanClose: Boolean);
    procedure TabView1InitTab(Sender: TObject; ATab: TTabViewTab);
    procedure TabView1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure chkRTLClick(Sender: TObject);
    procedure chkDraggableClick(Sender: TObject);
    procedure chkHintsClick(Sender: TObject);
    procedure chkMenuButtonClick(Sender: TObject);
    procedure chkCloseButtonClick(Sender: TObject);
    procedure btnLoadManyClick(Sender: TObject);
  private
    FTabCount: Integer;
    FSeed: TTabSeed;
    FEmptyCard: TCard;
    FTabMenu: TPopupMenu;
    FContextTab: TTabViewTab;
    procedure FillStyleList;
    procedure BuildTabMenu;
    procedure SeedInitialTabs;
    procedure AddSeededTab(const ACaption, ABody: string; AImageIndex: Integer;
      AModified: Boolean);
    function CreatePageContent(const ATitle, ABody: string): TCard;
    procedure UpdateActiveCard;
    procedure CloseTabsFrom(AFromIndex: Integer; AKeep: TTabViewTab);
    procedure TabMenuCloseClick(Sender: TObject);
    procedure TabMenuCloseOthersClick(Sender: TObject);
    procedure TabMenuCloseRightClick(Sender: TObject);
  public
  end;

var
  frmTabViewWorkspace: TfrmTabViewWorkspace;

implementation

{$R *.dfm}

const
  // TTabViewTab has no "modified" property, so the flag lives in Tag.
  cTabModified = 1;

function TabIsModified(ATab: TTabViewTab): Boolean;
begin
  Result := (ATab <> nil) and (ATab.Tag = cTabModified);
end;

procedure TfrmTabViewWorkspace.FillStyleList;
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

procedure TfrmTabViewWorkspace.BuildTabMenu;

  function NewItem(const ACaption: string; AHandler: TNotifyEvent): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Caption := ACaption;
    Result.OnClick := AHandler;
  end;

begin
  FTabMenu := TPopupMenu.Create(Self);
  FTabMenu.Items.Add(NewItem('&Close', TabMenuCloseClick));
  FTabMenu.Items.Add(NewItem('Close &others', TabMenuCloseOthersClick));
  FTabMenu.Items.Add(NewItem('Close to the &right', TabMenuCloseRightClick));
end;

function TfrmTabViewWorkspace.CreatePageContent(const ATitle, ABody: string): TCard;
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
  LBody.SetBounds(ScaleValue(24), ScaleValue(68), ScaleValue(520), ScaleValue(80));
  LBody.Caption := ABody;
  LBody.WordWrap := True;
  LBody.Font.Name := 'Segoe UI';
  LBody.Font.Height := -13;
  LBody.ParentFont := False;
end;

{ Queue the values, then let AddTab -> OnInitTab build the tab from them. }
procedure TfrmTabViewWorkspace.AddSeededTab(const ACaption, ABody: string;
  AImageIndex: Integer; AModified: Boolean);
begin
  FSeed.Active := True;
  FSeed.Caption := ACaption;
  FSeed.Body := ABody;
  FSeed.ImageIndex := AImageIndex;
  FSeed.Modified := AModified;
  try
    TabView1.AddTab;
  finally
    FSeed.Active := False;
  end;
end;

procedure TfrmTabViewWorkspace.SeedInitialTabs;
begin
  AddSeededTab('Overview',
    'TTabView hosts arbitrary controls per tab via the Control property - not only forms.',
    0, False);
  AddSeededTab('Analytics',
    'Tabs marked with an asterisk have unsaved changes and ask for confirmation before closing.',
    1, True);
  AddSeededTab('Design'#13#10'Notes',
    'Captions support multi-line text when TabOptions.WordWrap is enabled.',
    2, False);
  AddSeededTab('Devices',
    'Right-click any tab for Close / Close others / Close to the right.',
    3, True);
  AddSeededTab('Help',
    'Use "Load 20 tabs" to see the automatic scroll buttons and the tabs menu.',
    4, False);

  if TabView1.Tabs.Count > 0 then
    TabView1.TabIndex := 0;
  UpdateActiveCard;
end;

{ CardPanel always keeps the empty-state card, so ActiveCard is never nil. }
procedure TfrmTabViewWorkspace.UpdateActiveCard;
begin
  if (TabView1.ActiveTab <> nil) and (TabView1.ActiveTab.Control <> nil) then
    CardPanel1.ActiveCard := TCard(TabView1.ActiveTab.Control)
  else
    CardPanel1.ActiveCard := FEmptyCard;
end;

procedure TfrmTabViewWorkspace.FormCreate(Sender: TObject);
begin
  FTabCount := 0;
  FillStyleList;
  BuildTabMenu;

  FEmptyCard := CreatePageContent('No documents open',
    'Every tab has been closed. Use the + button to open a new one.');

  chkDraggable.Checked := TabView1.TabOptions.Draggable;
  chkHints.Checked := TabView1.TabOptions.ShowHints;
  chkMenuButton.Checked := TabView1.ShowTabsMenuButton;
  chkCloseButton.Checked := TabView1.TabOptions.ShowCloseButton;
end;

procedure TfrmTabViewWorkspace.FormShow(Sender: TObject);
begin
  if TabView1.Tabs.Count = 0 then
    SeedInitialTabs;
end;

procedure TfrmTabViewWorkspace.cmbStyleChange(Sender: TObject);
begin
  if cmbStyle.ItemIndex >= 0 then
    TStyleManager.SetStyle(cmbStyle.Items[cmbStyle.ItemIndex]);
end;

procedure TfrmTabViewWorkspace.btnLoadManyClick(Sender: TObject);
begin
  { Enough tabs to overflow the strip. Everything that follows is automatic:
    the scroll buttons appear (with click-and-hold repeat), the mouse wheel
    scrolls the strip, and the tabs menu starts nesting overflow into "..."
    submenus once the list is taller than the monitor work area. }
  for var I := 1 to 20 do
  begin
    Inc(FTabCount);
    AddSeededTab('Document ' + FTabCount.ToString,
      'One of many tabs, added to push the strip into overflow.',
      FTabCount mod VirtualImageList1.Count, False);
  end;
end;

procedure TfrmTabViewWorkspace.TabView1InitTab(Sender: TObject; ATab: TTabViewTab);
var
  LCaption, LBody: string;
  LImage: Integer;
  LModified: Boolean;
begin
  if FSeed.Active then
  begin
    LCaption := FSeed.Caption;
    LBody := FSeed.Body;
    LImage := FSeed.ImageIndex;
    LModified := FSeed.Modified;
  end
  else
  begin
    { Added by the "+" button. A tab created at runtime starts with an empty
      Caption - the "TabViewTabN" default only applies at design time. }
    Inc(FTabCount);
    LCaption := 'Document ' + FTabCount.ToString;
    LBody := 'Created by the + button. OnInitTab named it and built this page.';
    LImage := FTabCount mod VirtualImageList1.Count;
    LModified := False;
  end;

  { Unsaved documents are marked with a trailing asterisk - just a caption, no
    custom drawing. }
  if LModified then
  begin
    LCaption := LCaption + ' *';
    ATab.Tag := cTabModified;
  end
  else
    ATab.Tag := 0;

  ATab.Caption := LCaption;
  ATab.ImageIndex := LImage;
  ATab.Hint := StringReplace(LCaption, #13#10, ' ', [rfReplaceAll]);
  ATab.Control := CreatePageContent(ATab.Hint, LBody);
end;

procedure TfrmTabViewWorkspace.TabView1ChangeTab(Sender: TObject);
begin
  UpdateActiveCard;
end;

procedure TfrmTabViewWorkspace.TabView1CloseTab(Sender: TObject; ATab: TTabViewTab;
  var ACanClose: Boolean);
var
  LCard: TCard;
begin
  { Only unsaved tabs interrupt the user. Note this same handler also runs for
    programmatic DeleteTab calls, which is what lets "Close others" respect an
    unsaved document instead of silently discarding it. }
  ACanClose := True;
  if TabIsModified(ATab) then
    ACanClose := MessageDlg(
      Format('"%s" has unsaved changes.'#13#10'Close it anyway?',
        [StringReplace(ATab.Caption, #13#10, ' ', [rfReplaceAll])]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes;

  if ACanClose and (ATab.Control <> nil) then
  begin
    LCard := TCard(ATab.Control);
    ATab.Control := nil;
    LCard.Free;
  end;
end;

{ Closes tabs from AFromIndex to the end, skipping AKeep. }
procedure TfrmTabViewWorkspace.CloseTabsFrom(AFromIndex: Integer; AKeep: TTabViewTab);
var
  I, LBefore, LRefused: Integer;
begin
  LRefused := 0;
  { Backwards, because deleting index I never shifts a lower index. }
  for I := TabView1.Tabs.Count - 1 downto AFromIndex do
  begin
    if TabView1.Tabs[I] = AKeep then
      Continue;
    LBefore := TabView1.Tabs.Count;
    TabView1.DeleteTab(I);
    { DeleteTab routes through CloseTab, which fires the *vetoable* OnCloseTab.
      An unsaved tab can refuse, so never assume the loop emptied the strip. }
    if TabView1.Tabs.Count = LBefore then
      Inc(LRefused);
  end;

  UpdateActiveCard;
  if LRefused > 0 then
    ShowMessage(Format('%d tab(s) stayed open - still unsaved.', [LRefused]));
end;

procedure TfrmTabViewWorkspace.TabView1ContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  LPos: TPoint;
begin
  { TTabView has no per-tab menu property. TabFromPoint is the hit-testing API
    that makes a real per-tab context menu possible. }
  Handled := True;
  FContextTab := TabView1.TabFromPoint(MousePos);
  if FContextTab = nil then
    Exit; // right-click on empty strip - nothing to act on

  TabView1.TabIndex := FContextTab.Index;
  FTabMenu.Items[1].Enabled := TabView1.Tabs.Count > 1;
  FTabMenu.Items[2].Enabled := FContextTab.Index < TabView1.Tabs.Count - 1;

  LPos := TabView1.ClientToScreen(MousePos);
  FTabMenu.Popup(LPos.X, LPos.Y);
end;

procedure TfrmTabViewWorkspace.TabMenuCloseClick(Sender: TObject);
begin
  if FContextTab <> nil then
  begin
    TabView1.DeleteTab(FContextTab.Index);
    UpdateActiveCard;
  end;
end;

procedure TfrmTabViewWorkspace.TabMenuCloseOthersClick(Sender: TObject);
begin
  if FContextTab <> nil then
    CloseTabsFrom(0, FContextTab);
end;

procedure TfrmTabViewWorkspace.TabMenuCloseRightClick(Sender: TObject);
begin
  if FContextTab <> nil then
    CloseTabsFrom(FContextTab.Index + 1, nil);
end;

procedure TfrmTabViewWorkspace.chkRTLClick(Sender: TObject);
begin
  if chkRTL.Checked then
    BiDiMode := bdRightToLeft
  else
    BiDiMode := bdLeftToRight;
end;

procedure TfrmTabViewWorkspace.chkDraggableClick(Sender: TObject);
begin
  TabView1.TabOptions.Draggable := chkDraggable.Checked;
end;

procedure TfrmTabViewWorkspace.chkHintsClick(Sender: TObject);
begin
  TabView1.TabOptions.ShowHints := chkHints.Checked;
end;

procedure TfrmTabViewWorkspace.chkMenuButtonClick(Sender: TObject);
begin
  TabView1.ShowTabsMenuButton := chkMenuButton.Checked;
end;

procedure TfrmTabViewWorkspace.chkCloseButtonClick(Sender: TObject);
begin
  TabView1.TabOptions.ShowCloseButton := chkCloseButton.Checked;
end;

end.
