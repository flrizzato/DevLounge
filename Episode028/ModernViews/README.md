# TNavigationView & TTabView demos

Open [`ModernViews.groupproj`](ModernViews.groupproj) in the IDE.

Both controls ship in RAD Studio 13.2 (`Vcl.NavView`, `Vcl.TabView`).

## Demos

### 1. ModernShell: start here
Both controls in one app shell: `TNavigationView` rail on the left, `TTabView` document tabs across the content area, `TCardPanel` underneath. This is the pattern the two were designed for. They even share `Vcl.ViewCtrlUtil` for their scroll / menu / add buttons.

**Try:** pick a section in the rail (it opens a tab, or focuses the one already open); the hamburger to collapse the rail to icons; close *Reports* (asterisk = unsaved) and note the confirmation prompt; right-click any tab; switch to *Windows Modern Dark*.

### 2. NavigationView_AppShell
`TNavigationView` on its own, with enough items to overflow.

**Try:** the four `ItemSelectionOptions.Style` values (they transform the control); compact mode; scroll buttons, mouse wheel and Home/End once the rail overflows; drag reorder; hints; “Accent Settings item”, then switch to the *Themed* style and watch only half of it still apply (see Gotchas).

### 3. TabView_Workspace
Modern document tabs hosting controls (not only forms), with add / close / drag / overflow menu.

**Try:** “Load 20 tabs” to bring up the automatic scroll buttons and the tabs menu; drag to reorder; right-click a tab for *Close / Close others / Close to the right*; close *Analytics* or *Devices* (asterisk = unsaved) and note the confirmation; close every tab to reach the empty state; RTL.

### 4. TabView_TitleBar
Browser-style tabs on `TTitleBarPanel` (`Transparent` + `BackgroundHitTestTransparent`).

**Try:** drag the window from empty title-bar space; add / close / reorder tabs, switch VCL styles and the title-bar colours follow.

## How the pieces connect

- Content linking happens in `OnInitItem` / `OnInitTab` and `OnChangeItem` / `OnChangeTab` via each item’s `Control` property.
- `Item.Control` and `Tab.Control` are **inert storage slots**. Neither control shows, hides or parents anything for you, unlike `TPageControl`/`TTabSheet`, the page switching is yours to write.
- Everything on screen is stock component rendering. Unsaved documents are marked with a trailing asterisk in the tab caption.
