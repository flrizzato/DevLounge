unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.UITypes,
  System.Classes, System.Math, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus, Vcl.Themes, Vcl.WinXPanels,
  Vcl.BaseImageCollection, Vcl.ImageCollection, System.ImageList, Vcl.ImgList,
  Vcl.VirtualImageList, Vcl.NavView, Vcl.TabView;

type
  { The two controls are designed to work together: TNavigationView picks the
    section, TTabView holds the documents open inside it. Neither one shows or
    hides anything by itself - Item.Control and Tab.Control are inert storage
    slots, and the TCardPanel below is driven entirely from OnChangeItem /
    OnChangeTab. }
  TfrmModernShell = class(TForm)
    ImageCollection1: TImageCollection;
    VirtualImageList1: TVirtualImageList;
    pnlRail: TPanel;
    lblBrand: TLabel;
    NavigationView1: TNavigationView;
    pnlContent: TPanel;
    TabView1: TTabView;
    CardPanel1: TCardPanel;
    pnlOptions: TPanel;
    lblStyle: TLabel;
    cmbStyle: TComboBox;
    chkCompact: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmbStyleChange(Sender: TObject);
    procedure chkCompactClick(Sender: TObject);
    procedure NavigationView1InitItem(Sender: TObject; AItem: TNavigationViewItem);
    procedure NavigationView1ChangeItem(Sender: TObject);
    procedure NavigationView1MenuButtonClick(Sender: TObject);
    procedure TabView1ChangeTab(Sender: TObject);
    procedure TabView1CloseTab(Sender: TObject; ATab: TTabViewTab; var ACanClose: Boolean);
    procedure TabView1InitTab(Sender: TObject; ATab: TTabViewTab);
    procedure TabView1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
  private
    FEmptyCard: TCard;
    FTabMenu: TPopupMenu;
    FContextTab: TTabViewTab;
    FOpeningSection: Integer;   // nav index queued for the next OnInitTab
    FSyncing: Boolean;          // guards nav <-> tab feedback
    procedure FillStyleList;
    procedure BuildTabMenu;
    procedure UpdateRailWidth;
    procedure UpdateActiveCard;
    procedure OpenSection(AItemIndex: Integer);
    function FindTabForSection(AItemIndex: Integer): TTabViewTab;
    function CreateSectionCard(AItemIndex: Integer): TCard;
    procedure CloseTabsFrom(AFromIndex: Integer; AKeep: TTabViewTab);
    procedure TabMenuCloseClick(Sender: TObject);
    procedure TabMenuCloseOthersClick(Sender: TObject);
    procedure TabMenuCloseRightClick(Sender: TObject);
  public
  end;

var
  frmModernShell: TfrmModernShell;

implementation

{$R *.dfm}

const
  // Tab.Tag carries the nav index the tab belongs to, offset so that a
  // "modified" document can be flagged in the same slot.
  cSectionMask  = $FF;
  cModifiedFlag = $100;

  cSectionBody: array[0..4] of string = (
    'Documents opened from the rail become tabs here. Selecting a section ' +
    'that is already open focuses its tab instead of duplicating it.',
    'Reports carries unsaved changes in this demo - note the asterisk on the ' +
    'tab, and the confirmation when you try to close it.',
    'Right-click any tab for Close / Close others / Close to the right.',
    'Collapse the rail with the hamburger: TNavigationView resizes itself and ' +
    'switches to icons only.',
    'Both controls follow the active VCL style - try Windows Modern Dark.');

function SectionOf(ATab: TTabViewTab): Integer;
begin
  if ATab = nil then
    Result := -1
  else
    Result := ATab.Tag and cSectionMask;
end;

function TabIsModified(ATab: TTabViewTab): Boolean;
begin
  Result := (ATab <> nil) and (ATab.Tag and cModifiedFlag <> 0);
end;

procedure TfrmModernShell.FillStyleList;
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

procedure TfrmModernShell.BuildTabMenu;

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

function TfrmModernShell.CreateSectionCard(AItemIndex: Integer): TCard;
var
  LTitle, LBody: TLabel;
begin
  Result := CardPanel1.CreateNewCard;

  LTitle := TLabel.Create(Result);
  LTitle.Parent := Result;
  LTitle.AutoSize := False;
  LTitle.SetBounds(ScaleValue(28), ScaleValue(24), ScaleValue(440), ScaleValue(38));
  LTitle.Caption := NavigationView1.Items[AItemIndex].Caption;
  LTitle.Font.Name := 'Segoe UI Semibold';
  LTitle.Font.Height := -27;
  LTitle.ParentFont := False;

  LBody := TLabel.Create(Result);
  LBody.Parent := Result;
  LBody.AutoSize := False;
  LBody.SetBounds(ScaleValue(28), ScaleValue(72), ScaleValue(560), ScaleValue(90));
  LBody.Caption := cSectionBody[AItemIndex];
  LBody.WordWrap := True;
  LBody.Font.Name := 'Segoe UI';
  LBody.Font.Height := -13;
  LBody.ParentFont := False;
end;

procedure TfrmModernShell.UpdateRailWidth;
begin
  // CompactMode resizes the nav view itself; pnlRail hosts a brand header
  // above it, so the panel has to follow.
  if NavigationView1.CompactMode then
  begin
    pnlRail.Width := NavigationView1.ItemsMargins.Left +
      NavigationView1.ItemHeight + NavigationView1.ItemsMargins.Right;
    lblBrand.Visible := False;
  end
  else
  begin
    pnlRail.Width := NavigationView1.NormalWidth;
    lblBrand.Visible := True;
  end;
  chkCompact.Checked := NavigationView1.CompactMode;
end;

procedure TfrmModernShell.UpdateActiveCard;
begin
  if (TabView1.ActiveTab <> nil) and (TabView1.ActiveTab.Control <> nil) then
    CardPanel1.ActiveCard := TCard(TabView1.ActiveTab.Control)
  else
    CardPanel1.ActiveCard := FEmptyCard;   // every tab closed
end;

function TfrmModernShell.FindTabForSection(AItemIndex: Integer): TTabViewTab;
begin
  for var I := 0 to TabView1.Tabs.Count - 1 do
    if SectionOf(TabView1.Tabs[I]) = AItemIndex then
      Exit(TabView1.Tabs[I]);
  Result := nil;
end;

{ Focus the section's tab if it is already open, otherwise open one. }
procedure TfrmModernShell.OpenSection(AItemIndex: Integer);
var
  LTab: TTabViewTab;
begin
  if (AItemIndex < 0) or (AItemIndex >= NavigationView1.Items.Count) then
    Exit;

  LTab := FindTabForSection(AItemIndex);
  if LTab <> nil then
  begin
    TabView1.TabIndex := LTab.Index;
    Exit;
  end;

  { AddTab cannot carry data into the tab it creates, so the section is queued
    here and consumed by OnInitTab. }
  FOpeningSection := AItemIndex;
  try
    TabView1.AddTab;   // fires OnInitTab, then activates the new tab
  finally
    FOpeningSection := -1;
  end;
end;

procedure TfrmModernShell.FormCreate(Sender: TObject);
begin
  FOpeningSection := -1;
  FillStyleList;
  BuildTabMenu;

  FEmptyCard := CardPanel1.CreateNewCard;
  var LEmpty := TLabel.Create(FEmptyCard);
  LEmpty.Parent := FEmptyCard;
  LEmpty.AutoSize := False;
  LEmpty.SetBounds(ScaleValue(28), ScaleValue(24), ScaleValue(520), ScaleValue(60));
  LEmpty.Caption := 'No documents open.'#13#10'Pick a section in the rail to open one.';
  LEmpty.WordWrap := True;
  LEmpty.Font.Name := 'Segoe UI';
  LEmpty.Font.Height := -15;
  LEmpty.Font.Color := clGrayText;
  LEmpty.ParentFont := False;

  UpdateRailWidth;
end;

procedure TfrmModernShell.FormShow(Sender: TObject);
begin
  pnlRail.Color := StyleServices(Self).GetSystemColor(clWindow);
  if TabView1.Tabs.Count = 0 then
  begin
    OpenSection(0);
    OpenSection(1);
    NavigationView1.ItemIndex := 0;
    TabView1.TabIndex := 0;
  end;
  UpdateActiveCard;
end;

procedure TfrmModernShell.cmbStyleChange(Sender: TObject);
begin
  if cmbStyle.ItemIndex >= 0 then
  begin
    TStyleManager.SetStyle(cmbStyle.Items[cmbStyle.ItemIndex]);
    pnlRail.Color := StyleServices(Self).GetSystemColor(clWindow);
  end;
end;

procedure TfrmModernShell.chkCompactClick(Sender: TObject);
begin
  NavigationView1.CompactMode := chkCompact.Checked;
  UpdateRailWidth;
end;

procedure TfrmModernShell.NavigationView1MenuButtonClick(Sender: TObject);
begin
  UpdateRailWidth;
end;

procedure TfrmModernShell.NavigationView1InitItem(Sender: TObject;
  AItem: TNavigationViewItem);
begin
  if AItem.Hint = '' then
    AItem.Hint := AItem.Caption;
end;

procedure TfrmModernShell.NavigationView1ChangeItem(Sender: TObject);
begin
  if FSyncing then
    Exit;
  FSyncing := True;
  try
    OpenSection(NavigationView1.ItemIndex);
  finally
    FSyncing := False;
  end;
end;

procedure TfrmModernShell.TabView1InitTab(Sender: TObject; ATab: TTabViewTab);
var
  LSection: Integer;
begin
  LSection := FOpeningSection;
  if LSection < 0 then
    LSection := Max(NavigationView1.ItemIndex, 0);

  ATab.Caption := NavigationView1.Items[LSection].Caption;
  ATab.ImageIndex := NavigationView1.Items[LSection].ImageIndex;
  ATab.Tag := LSection;
  if LSection = 1 then
  begin
    ATab.Tag := ATab.Tag or cModifiedFlag;
    ATab.Caption := ATab.Caption + ' *';
  end;
  ATab.Hint := ATab.Caption;
  ATab.Control := CreateSectionCard(LSection);
end;

procedure TfrmModernShell.TabView1ChangeTab(Sender: TObject);
begin
  UpdateActiveCard;

  { Keep the rail in step with the active document. FSyncing stops this from
    bouncing back into OnChangeItem and reopening the tab. }
  if FSyncing or (TabView1.ActiveTab = nil) then
    Exit;
  FSyncing := True;
  try
    NavigationView1.ItemIndex := SectionOf(TabView1.ActiveTab);
  finally
    FSyncing := False;
  end;
end;

procedure TfrmModernShell.TabView1CloseTab(Sender: TObject; ATab: TTabViewTab;
  var ACanClose: Boolean);
var
  LCard: TCard;
begin
  ACanClose := True;
  if TabIsModified(ATab) then
    ACanClose := MessageDlg(
      Format('"%s" has unsaved changes.'#13#10'Close it anyway?', [ATab.Caption]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes;

  if ACanClose and (ATab.Control <> nil) then
  begin
    LCard := TCard(ATab.Control);
    ATab.Control := nil;
    LCard.Free;
  end;
end;

procedure TfrmModernShell.CloseTabsFrom(AFromIndex: Integer; AKeep: TTabViewTab);
var
  I, LBefore, LRefused: Integer;
begin
  LRefused := 0;
  { Backwards - deleting index I never shifts a lower index. }
  for I := TabView1.Tabs.Count - 1 downto AFromIndex do
  begin
    if TabView1.Tabs[I] = AKeep then
      Continue;
    LBefore := TabView1.Tabs.Count;
    TabView1.DeleteTab(I);
    { DeleteTab routes through the vetoable OnCloseTab, so an unsaved document
      can refuse - never assume the loop emptied the strip. }
    if TabView1.Tabs.Count = LBefore then
      Inc(LRefused);
  end;

  UpdateActiveCard;
  if LRefused > 0 then
    ShowMessage(Format('%d tab(s) stayed open - still unsaved.', [LRefused]));
end;

procedure TfrmModernShell.TabView1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  LPos: TPoint;
begin
  Handled := True;
  FContextTab := TabView1.TabFromPoint(MousePos);
  if FContextTab = nil then
    Exit;

  TabView1.TabIndex := FContextTab.Index;
  FTabMenu.Items[1].Enabled := TabView1.Tabs.Count > 1;
  FTabMenu.Items[2].Enabled := FContextTab.Index < TabView1.Tabs.Count - 1;

  LPos := TabView1.ClientToScreen(MousePos);
  FTabMenu.Popup(LPos.X, LPos.Y);
end;

procedure TfrmModernShell.TabMenuCloseClick(Sender: TObject);
begin
  if FContextTab <> nil then
  begin
    TabView1.DeleteTab(FContextTab.Index);
    UpdateActiveCard;
  end;
end;

procedure TfrmModernShell.TabMenuCloseOthersClick(Sender: TObject);
begin
  if FContextTab <> nil then
    CloseTabsFrom(0, FContextTab);
end;

procedure TfrmModernShell.TabMenuCloseRightClick(Sender: TObject);
begin
  if FContextTab <> nil then
    CloseTabsFrom(FContextTab.Index + 1, nil);
end;

end.
