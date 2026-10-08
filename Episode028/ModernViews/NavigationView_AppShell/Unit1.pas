unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.BaseImageCollection,
  Vcl.ImageCollection, Vcl.VirtualImage, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList, Vcl.Buttons,
  Vcl.WinXPanels, Vcl.Themes, Vcl.NavView, Vcl.WinXCtrls;

type
  TfrmNavViewAppShell = class(TForm)
    ImageCollection1: TImageCollection;
    VirtualImageList1: TVirtualImageList;
    pnlRail: TPanel;
    lblBrand: TLabel;
    imgLogo: TVirtualImage;
    NavigationView1: TNavigationView;
    btnExit: TSpeedButton;
    pnlClient: TPanel;
    pnlOptions: TPanel;
    lblStyle: TLabel;
    cmbStyle: TComboBox;
    lblSelection: TLabel;
    cmbSelectionStyle: TComboBox;
    chkCompact: TCheckBox;
    chkMenuButton: TCheckBox;
    chkDraggable: TCheckBox;
    chkHints: TCheckBox;
    chkAccent: TCheckBox;
    lblAccentNote: TLabel;
    CardPanel1: TCardPanel;
    Card1: TCard;
    lblDataTitle: TLabel;
    lblDataBody: TLabel;
    edtSample: TEdit;
    Card2: TCard;
    lblReportsTitle: TLabel;
    lblReportsBody: TLabel;
    btnSampleReport: TButton;
    Card3: TCard;
    lblSettingsTitle: TLabel;
    lblSettingsBody: TLabel;
    chkSampleSetting: TCheckBox;
    Card4: TCard;
    lblUsersTitle: TLabel;
    lblUsersBody: TLabel;
    lstUsers: TListBox;
    Card5: TCard;
    lblAboutTitle: TLabel;
    lblAboutBody: TLabel;
    Card6: TCard;
    lblArchiveTitle: TLabel;
    lblArchiveBody: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmbStyleChange(Sender: TObject);
    procedure cmbSelectionStyleChange(Sender: TObject);
    procedure NavigationView1InitItem(Sender: TObject; AItem: TNavigationViewItem);
    procedure NavigationView1ChangeItem(Sender: TObject);
    procedure NavigationView1DrawItemContent(Sender: TObject; AItem: TNavigationViewItem;
      ACanvas: TCanvas; ADrawRect: TRect; var ADrawDefault: Boolean);
    procedure NavigationView1GetItemSelectionColors(Sender: TObject;
      AItem: TNavigationViewItem; var ASelectionColor, ASelectionLineColor,
      ATextColor: TColor; var ASelectionColorAlpha: Byte);
    procedure chkCompactClick(Sender: TObject);
    procedure chkMenuButtonClick(Sender: TObject);
    procedure chkDraggableClick(Sender: TObject);
    procedure chkHintsClick(Sender: TObject);
    procedure chkAccentClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure NavigationView1MenuButtonClick(Sender: TObject);
  private
    FDesignCardCount: Integer;
    FPlaceholderCard: TCard;
    FPlaceholderTitle: TLabel;
    FPlaceholderBody: TLabel;
    procedure FillStyleList;
    procedure SyncOptionChecks;
    procedure UpdateRailWidth;
    procedure UpdateAccentNote;
    procedure UpdateActiveCard;
    function CreatePlaceholderCard: TCard;
  public
  end;

var
  frmNavViewAppShell: TfrmNavViewAppShell;

implementation

{$R *.dfm}

procedure TfrmNavViewAppShell.FillStyleList;
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

procedure TfrmNavViewAppShell.SyncOptionChecks;
begin
  chkCompact.Checked := NavigationView1.CompactMode;
  chkMenuButton.Checked := NavigationView1.ShowMenuButton;
  chkDraggable.Checked := NavigationView1.ItemOptions.Draggable;
  chkHints.Checked := NavigationView1.ItemOptions.ShowHints;
end;

procedure TfrmNavViewAppShell.UpdateRailWidth;
var
  LWidth: Integer;
begin
  // CompactMode resizes TNavigationView itself. Here the control is alClient
  // inside pnlRail (so the rail can carry a brand header), which means the
  // host panel has to follow it. Dock the nav view alLeft directly and this
  // whole routine becomes unnecessary.
  if NavigationView1.CompactMode then
  begin
    LWidth := NavigationView1.ItemsMargins.Left + NavigationView1.ItemHeight +
      NavigationView1.ItemsMargins.Right;
    lblBrand.Visible := False;
    imgLogo.Visible := False;
  end
  else
  begin
    LWidth := NavigationView1.NormalWidth;
    if LWidth < 1 then
      LWidth := 200;
    lblBrand.Visible := True;
    imgLogo.Visible := True;
  end;
  pnlRail.Width := LWidth;
end;

procedure TfrmNavViewAppShell.UpdateAccentNote;
begin
  { The two accent hooks have different reach, which is the whole point of this
    toggle:

      OnDrawItemContent        - runs for every item in every state, so the
                                 caption colour it sets always applies.
      OnGetItemSelectionColors - only invoked from the Color / ModernRect /
                                 ModernRounded painters, and only while the
                                 item is hot or selected.

    Under nvissThemed the second one is never called at all, so the selection
    colour silently stays themed while the caption still turns red. }
  if NavigationView1.ItemSelectionOptions.Style = nvissThemed then
    lblAccentNote.Caption :=
      'Themed style: caption colour still applies; OnGetItemSelectionColors is never called.'
  else
    lblAccentNote.Caption := '';
  NavigationView1.Invalidate;
end;

{ One reusable page for the nav items that exist only to overflow the rail. }
function TfrmNavViewAppShell.CreatePlaceholderCard: TCard;
begin
  Result := CardPanel1.CreateNewCard;

  FPlaceholderTitle := TLabel.Create(Result);
  FPlaceholderTitle.Parent := Result;
  FPlaceholderTitle.AutoSize := False;
  FPlaceholderTitle.SetBounds(ScaleValue(24), ScaleValue(20), ScaleValue(420), ScaleValue(36));
  FPlaceholderTitle.Font.Name := 'Segoe UI Semibold';
  FPlaceholderTitle.Font.Height := -27;
  FPlaceholderTitle.ParentFont := False;

  FPlaceholderBody := TLabel.Create(Result);
  FPlaceholderBody.Parent := Result;
  FPlaceholderBody.AutoSize := False; // keep the fixed block; let WordWrap fill it
  FPlaceholderBody.SetBounds(ScaleValue(24), ScaleValue(68), ScaleValue(520), ScaleValue(80));
  FPlaceholderBody.WordWrap := True;
  FPlaceholderBody.Font.Name := 'Segoe UI';
  FPlaceholderBody.Font.Height := -13;
  FPlaceholderBody.ParentFont := False;
  FPlaceholderBody.Caption :=
    'These extra entries exist so the rail overflows. Scroll buttons appear ' +
    'automatically, the mouse wheel scrolls, and Home/End jump to either end.';
end;

procedure TfrmNavViewAppShell.FormCreate(Sender: TObject);
begin
  FDesignCardCount := CardPanel1.CardCount;
  FPlaceholderCard := CreatePlaceholderCard;

  { OnInitItem already ran once during Loaded, before the placeholder card
    existed. InitItems re-runs it now that it does. }
  NavigationView1.InitItems;

  FillStyleList;
  cmbSelectionStyle.Items.Clear;
  cmbSelectionStyle.Items.Add('Themed');
  cmbSelectionStyle.Items.Add('Color');
  cmbSelectionStyle.Items.Add('Modern rect');
  cmbSelectionStyle.Items.Add('Modern rounded');
  cmbSelectionStyle.ItemIndex := Ord(NavigationView1.ItemSelectionOptions.Style);

  SyncOptionChecks;
  UpdateRailWidth;
  UpdateAccentNote;
  { CreateNewCard activated the placeholder as a side effect - put the card
    matching the current ItemIndex back in front. }
  UpdateActiveCard;

  lstUsers.Items.Clear;
  lstUsers.Items.Add('Alex Rivera');
  lstUsers.Items.Add('Jordan Lee');
  lstUsers.Items.Add('Sam Patel');
end;

procedure TfrmNavViewAppShell.FormShow(Sender: TObject);
begin
  pnlRail.Color := StyleServices(Self).GetSystemColor(clWindow);
end;

procedure TfrmNavViewAppShell.cmbStyleChange(Sender: TObject);
begin
  if cmbStyle.ItemIndex >= 0 then
  begin
    TStyleManager.SetStyle(cmbStyle.Items[cmbStyle.ItemIndex]);
    pnlRail.Color := StyleServices(Self).GetSystemColor(clWindow);
  end;
end;

procedure TfrmNavViewAppShell.cmbSelectionStyleChange(Sender: TObject);
begin
  if cmbSelectionStyle.ItemIndex >= 0 then
    NavigationView1.ItemSelectionOptions.Style :=
      TNavigationViewItemSelectionStyle(cmbSelectionStyle.ItemIndex);
  UpdateAccentNote;
end;

procedure TfrmNavViewAppShell.chkCompactClick(Sender: TObject);
begin
  NavigationView1.CompactMode := chkCompact.Checked;
  UpdateRailWidth;
end;

procedure TfrmNavViewAppShell.chkMenuButtonClick(Sender: TObject);
begin
  NavigationView1.ShowMenuButton := chkMenuButton.Checked;
end;

procedure TfrmNavViewAppShell.chkDraggableClick(Sender: TObject);
begin
  NavigationView1.ItemOptions.Draggable := chkDraggable.Checked;
end;

procedure TfrmNavViewAppShell.chkHintsClick(Sender: TObject);
begin
  NavigationView1.ItemOptions.ShowHints := chkHints.Checked;
end;

procedure TfrmNavViewAppShell.chkAccentClick(Sender: TObject);
begin
  NavigationView1.Invalidate;
end;

procedure TfrmNavViewAppShell.btnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmNavViewAppShell.NavigationView1InitItem(Sender: TObject;
  AItem: TNavigationViewItem);
begin
  AItem.Tag := AItem.Index;
  if AItem.Hint = '' then
    AItem.Hint := StringReplace(AItem.Caption, #13#10, ' - ', [rfReplaceAll]);

  { Item.Control is just a storage slot - nothing is shown or hidden.
    OnChangeItem is what turns it into the visible page. }
  if AItem.Index < FDesignCardCount then
    AItem.Control := CardPanel1.Cards[AItem.Index]
  else
    AItem.Control := FPlaceholderCard;
end;

procedure TfrmNavViewAppShell.UpdateActiveCard;
var
  LItem: TNavigationViewItem;
begin
  LItem := NavigationView1.ActiveItem;
  if (LItem = nil) or (LItem.Control = nil) then
    Exit;

  if LItem.Control = FPlaceholderCard then
    FPlaceholderTitle.Caption := StringReplace(LItem.Caption, #13#10, ' ', [rfReplaceAll]);

  CardPanel1.ActiveCard := TCard(LItem.Control);
end;

procedure TfrmNavViewAppShell.NavigationView1ChangeItem(Sender: TObject);
begin
  UpdateActiveCard;
end;

procedure TfrmNavViewAppShell.NavigationView1MenuButtonClick(Sender: TObject);
begin
  SyncOptionChecks;
  UpdateRailWidth;
end;

procedure TfrmNavViewAppShell.NavigationView1DrawItemContent(Sender: TObject;
  AItem: TNavigationViewItem; ACanvas: TCanvas; ADrawRect: TRect;
  var ADrawDefault: Boolean);
begin
  if chkAccent.Checked and (AItem.Caption = 'Settings') then
    ACanvas.Font.Color := clRed;
end;

procedure TfrmNavViewAppShell.NavigationView1GetItemSelectionColors(Sender: TObject;
  AItem: TNavigationViewItem; var ASelectionColor, ASelectionLineColor,
  ATextColor: TColor; var ASelectionColorAlpha: Byte);
begin
  if chkAccent.Checked and (AItem.Caption = 'Settings') then
  begin
    ASelectionColor := clRed;
    ASelectionLineColor := clRed;
    ATextColor := clRed;
    ASelectionColorAlpha := 30;
  end;
end;

end.
