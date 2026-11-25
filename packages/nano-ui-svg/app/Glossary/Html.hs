-- | Lucid HTML for the glossary page and the JSON values the book's
-- @nano-ui-banks.js@ carries.
module Glossary.Html
  ( buildHtmlDocument
  , buildSvgBanksValue
  , buildTrapsValue
  , buildWidgetMetaValue
  ) where

import Data.Aeson.Micro (ToJSON (..), Value, object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Glossary.Render (Assets (..), encodeJsonText)
import Glossary.Specimens
  ( InteractionCard (..)
  , ProbeRow (..)
  , ToggleNote (..)
  , TypoDemo (..)
  , TypoSectionData (..)
  , impactBadgeText
  , impactCode
  )
import Lucid
import Lucid.Base (makeAttributes)

-- ---------------------------------------------------------------------------
-- HTML Generation
-- ---------------------------------------------------------------------------

-- | 'show' as text, for ids and attribute values.
-- | Escaped text as HTML content.
txt :: Text -> Html ()
txt = toHtml

showT :: Show a => a -> Text
showT = T.pack . show

-- | Pointer enter/leave handlers, which Lucid does not name.
onmouseenter_ :: Text -> Attributes
onmouseenter_ = makeAttributes "onmouseenter"

onmouseleave_ :: Text -> Attributes
onmouseleave_ = makeAttributes "onmouseleave"

-- | Snippet markup for prose and table cells: escaped text, @<code>@, surface
-- badges, line breaks, and links. Interaction notes spell out identifiers with
-- @`backticks`@; 'prose' turns those into code elements rather than requiring
-- hand-written markup in every string.
data Snippet
  = Txt !Text
  | Code !Text
  | Badge !Text !Text
  | Brk
  | Link !Text ![Snippet]

inline :: [Snippet] -> Html ()
inline = mapM_ go
  where
    go = \case
      Txt t -> toHtml t
      Code t -> code_ (toHtml t)
      Badge cls t -> span_ [class_ ("badge " <> cls)] (toHtml t)
      Brk -> br_ []
      Link href xs -> a_ [href_ href] (inline xs)

-- | Render @`backticked`@ runs as @<code>@ and everything else as escaped text.
prose :: Text -> Html ()
prose t = sequence_ [if odd i then code_ (toHtml piece) else toHtml piece | (i, piece) <- zip [0 :: Int ..] (T.splitOn "`" t)]

-- | A section heading row: inline-markup title and summary.
sectionHead :: [Snippet] -> [Snippet] -> Html ()
sectionHead title summary = div_ [class_ "section-head"] $ do
  h2_ (inline title)
  span_ (inline summary)

-- | Section I shell: the state/focus workbench. The frames themselves are
-- pre-rendered; the script swaps them into @#state-svg-well@.
stateStripSection :: Text -> Html ()
stateStripSection baseSvg = do
  div_ [class_ "workbench-card", id_ "state-workbench"] $ do
    div_ [class_ "wb-grid"] $ do
      div_ [class_ "wb-viewport-col"] $ do
        div_ [class_ "wb-viewport-head"] $ do
          span_ [class_ "wb-active-label", id_ "state-active-label"] "State: Resting · defaultTheme"
          button_ [type_ "button", class_ "mini-btn", onclick_ "resetStateWorkbench()"] "Reset"
        div_ [class_ "svg-well wb-svg-stage", id_ "state-svg-well"] (toHtmlRaw baseSvg)
        div_ [class_ "wb-code-bar"] (code_ [id_ "state-code-preview"] "-- Resting state under defaultTheme")
      div_ [class_ "wb-controls-col"] $ do
        div_ [class_ "control-group-label"] "1. Choose a state:"
        div_ [class_ "state-pill-grid", id_ "state-mode-pills"] $
          sequence_ [statePill i title detail | (i, (title, detail)) <- zip [0 :: Int ..] statePills]
        div_ [class_ "control-group-label", style_ "margin-top:12px"] "2. Toggle modifiers:"
        div_ [class_ "toggle-chip-grid", id_ "state-style-toggles"] $
          mapM_ stateStyleChip stateChips
        div_ [class_ "wb-inspector", id_ "state-inspector"] mempty
  where
    statePills =
      [ ("Resting", "styleBg + styleBorder")
      , ("Hovered", "styleHoverBg")
      , ("Pressed", "styleActiveBg")
      , ("Focused", "outer themeFocusRing")
      , ("Focused input", "accent border, active fill")
      , ("Open select", "button box, popup menu")
      ]
    statePill :: Int -> Text -> Text -> Html ()
    statePill i title detail =
      button_
        [ type_ "button"
        , class_ (if i == 0 then "state-pill active" else "state-pill")
        , data_ "state" (showT i)
        , onclick_ ("setStateMode(" <> showT i <> ")")
        ]
        $ do
          strong_ (toHtml title)
          span_ (toHtml detail)
    stateChips =
      [ (0 :: Int, "buttonStyle", "btn", "impact-primary", "themeButton")
      , (1, "inputStyle", "inp", "impact-primary", "themeInput")
      , (2, "popupStyle", "pop", "impact-partial", "Open menu")
      , (3, "accentColor", "acc", "impact-partial", "Focus")
      , (4, "disabledWhen", "dis", "impact-surprise", "Blocked")
      ]
    stateStyleChip :: (Int, Text, Text, Text, Text) -> Html ()
    stateStyleChip (bit, name, probe, toneCls, tag) =
      styleChip
        [ data_ "sbit" (showT bit)
        , data_ "probe" probe
        , onclick_ ("toggleStateStyle(" <> showT bit <> ")")
        ]
        name
        (Just (toneCls, tag))


-- | A style toggle chip: checkbox square, label, and an optional impact tag.
-- @attrs@ carry the per-workbench data attributes and click handler.
styleChip :: [Attributes] -> Text -> Maybe (Text, Text) -> Html ()
styleChip attrs name mTag =
  button_ ([type_ "button", class_ "style-chip"] <> attrs) $ do
    span_ [class_ "chip-check"] mempty
    span_ [class_ "chip-name"] (toHtml name)
    case mTag of
      Just (toneCls, tag) -> span_ [class_ ("chip-tag " <> toneCls)] (toHtml tag)
      Nothing -> mempty

-- | A small action button.
miniBtn :: [Attributes] -> Text -> Html ()
miniBtn attrs btnLabel = button_ ([type_ "button", class_ "mini-btn"] <> attrs) (toHtml btnLabel)

-- | One per-widget workbench card: metadata, live expression, stage, and the
-- seven style toggles.
probeRowCard :: ProbeRow -> Html ()
probeRowCard pr = do
  article_
    [ class_ "probe-row filter-item"
    , id_ ("widget-" <> wid)
    , data_ "wid" wid
    , data_ "cat" (prCategory pr)
    ]
    $ do
      div_ [class_ "probe-card-grid"] $ do
        div_ [class_ "probe-meta"] $ do
          div_ [class_ "probe-title-line"] $ do
            h3_ [class_ "probe-title"] (toHtml (prTitle pr))
            span_ [class_ "surface-pill"] (toHtml (prPrimarySurface pr))
          code_ [class_ "probe-sig"] (toHtml (prSignature pr))
          p_ [class_ "probe-desc"] (prose (prSummary pr))
          div_ [class_ "wb-code-bar"] $ do
            div_ [class_ "code-bar-head"] (span_ "Live Haskell Expression")
            pre_ (code_ [id_ ("code-" <> wid)] (toHtml (prCodeExpr pr)))
        div_ [class_ "probe-stage-col"] $ do
          div_ [class_ "wb-viewport-head"] $ do
            span_ [class_ "wb-active-label", id_ ("status-" <> wid)] "defaultTheme (0 modifiers)"
            div_ [class_ "wb-head-actions"] $ do
              miniBtn
                [ id_ ("cmp-btn-" <> wid)
                , onclick_ ("toggleWidgetCompare('" <> wid <> "')")
                , title_ "Show defaultTheme baseline next to active SVG"
                ]
                "⇄ Compare"
              miniBtn [onclick_ ("setWidgetMask('" <> wid <> "', 0)")] "Reset"
          div_ [class_ "wb-dual-stage", id_ ("dual-" <> wid)] $ do
            div_ [class_ "svg-well wb-baseline-well hidden-compare", id_ ("base-well-" <> wid)] $ do
              div_ [class_ "well-corner-tag"] "Baseline (defaultTheme)"
              div_ [class_ "well-svg-inner", id_ ("base-svg-" <> wid)] mempty
            div_ [class_ "svg-well wb-active-well", id_ ("svg-" <> wid)] (toHtmlRaw (prBaselineSvg pr))
        div_ [class_ "probe-controls-col"] $ do
          div_ [class_ "control-group-label"] "Toggle modifiers:"
          div_ [class_ "toggle-chip-grid"] $ mapM_ (toggleChip wid) (prToggles pr)
          div_ [class_ "wb-inspector", id_ ("notes-" <> wid)] mempty
  where
    wid = prId pr
    toggleChip :: Text -> ToggleNote -> Html ()
    toggleChip w tn =
      styleChip
        [ data_ "wid" w
        , data_ "bit" (showT (tnBit tn))
        , data_ "probe" (tnKey tn)
        , data_ "impact" (impactCode (tnImpact tn))
        , onmouseenter_ ("previewWidgetNote('" <> w <> "'," <> showT (tnBit tn) <> ")")
        , onmouseleave_ ("clearWidgetPreview('" <> w <> "')")
        , onclick_ ("toggleWidgetBit('" <> w <> "'," <> showT (tnBit tn) <> ")")
        ]
        (tnShort tn)
        (Just ("impact-" <> impactCode (tnImpact tn), impactBadgeText (tnImpact tn)))

-- | Section IV: the 64-combination typography workbench plus reference cards.
typoSection :: TypoSectionData -> Html ()
typoSection tsd = do
  div_
    [ class_ "workbench-card filter-item"
    , data_ "cat" "typography"
    , id_ "typo-workbench"
    , style_ "margin-bottom:16px"
    ]
    $ do
      div_ [class_ "wb-grid"] $ do
        div_ [class_ "wb-viewport-col"] $ do
          div_ [class_ "wb-viewport-head"] $ do
            span_ [class_ "wb-active-label", id_ "typo-active-label"] "No modifiers"
            miniBtn [onclick_ "setTypoMask(0)"] "Reset"
          div_ [class_ "svg-well wb-svg-stage", id_ "typo-svg-well"] (toHtmlRaw (tsdBaseSvg tsd))
          div_ [class_ "wb-code-bar"] (code_ [id_ "typo-code-preview"] "let m = id -- toggle modifiers on the right")
        div_ [class_ "wb-controls-col"] $ do
          div_ [class_ "control-group-label"] "Toggle modifiers:"
          p_ [style_ "margin:0 0 10px 0;font-size:13px;color:var(--ink-secondary)"] $
            inline
              [ Code "labelWith", Txt ", ", Code "textInput", Txt ", ", Code "buttonContent"
              , Txt " take every modifier, and so does ", Code "buttonWith", Txt "."
              ]
          div_ [class_ "toggle-chip-grid", id_ "typo-toggles"] $ mapM_ typoChip typoChips
          div_ [style_ "margin-top:10px;display:flex;gap:6px;flex-wrap:wrap"] $ do
            miniBtn [onclick_ "setTypoMask(63)"] "Enable all"
            miniBtn [onclick_ "setTypoMask(21)"] "Bold, underline, danger"
            miniBtn [onclick_ "setTypoMask(32)"] "fontSize 16"
  mapM_ typoCard (tsdDemos tsd)
  where
    typoChips =
      [ (0 :: Int, "fontBold", "impact-primary", "All rows")
      , (1, "fontItalic", "impact-primary", "All rows")
      , (2, "fontUnderline", "impact-primary", "All rows")
      , (3, "fontStrike", "impact-primary", "All rows")
      , (4, "fontTone Danger", "impact-primary", "All rows")
      , (5, "fontSize 16", "impact-primary", "All rows")
      ]
    typoChip :: (Int, Text, Text, Text) -> Html ()
    typoChip (bit, name, toneCls, tag) =
      styleChip
        [ data_ "tbit" (showT bit)
        , onclick_ ("toggleTypoBit(" <> showT bit <> ")")
        ]
        name
        (Just (toneCls, tag))
    typoCard :: TypoDemo -> Html ()
    typoCard d =
      div_ [class_ "typo-card filter-item", data_ "cat" "typography"] $ do
        div_ [class_ "typo-info"] $ do
          h3_ (toHtml (tdTitle d))
          code_ [class_ "typo-code"] (toHtml (tdCode d))
          p_ (toHtml (tdNote d))
        div_ [class_ "svg-well typo-svg"] (toHtmlRaw (tdSvg d))

-- | Section V: the eight unexpected-interaction cards.
interactionsSection :: [InteractionCard] -> Html ()
interactionsSection = mapM_ trapCard
  where
    trapCard :: InteractionCard -> Html ()
    trapCard c = do
      let tid = icNumber c
      article_
        [ class_ "trap-card filter-item"
        , id_ ("trap-" <> tid)
        , data_ "cat" ("trap " <> icCategory c)
        ]
        $ do
          div_ [class_ "trap-header"] $ do
            div_ [style_ "display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:8px"] $ do
              span_ [class_ "trap-num"] ("INTERACTION #" <> toHtml tid)
              div_ [class_ "trap-mode-bar", data_ "trap" tid] $ do
                button_ [type_ "button", class_ "mini-btn active", onclick_ ("setTrapView('" <> tid <> "','bad',this)")] "⚠ Show Trap"
                button_ [type_ "button", class_ "mini-btn", onclick_ ("setTrapView('" <> tid <> "','good',this)")] "✓ Show Fix"
                button_ [type_ "button", class_ "mini-btn", onclick_ ("setTrapView('" <> tid <> "','split',this)")] "⇄ Side-by-Side"
            h3_ [class_ "trap-title"] (toHtml (icTitle c))
            code_ [class_ "trap-where"] (toHtml (icWhere c))
          div_ [class_ "trap-body"] $ do
            div_ [class_ "trap-prose"] $ do
              div_ [class_ "trap-block"] $ do
                strong_ "Why it happens:"
                txt " "
                prose (icWhy c)
              div_ [class_ "trap-block trap-fix"] $ do
                strong_ "How to handle it:"
                txt " "
                prose (icFix c)
              div_ [class_ "trap-code-grid"] $ do
                div_ [class_ "code-pane code-bad"] $ do
                  div_ [class_ "code-tag"] "Unexpected / Trap"
                  pre_ (code_ (toHtml (icCodeBad c)))
                div_ [class_ "code-pane code-good"] $ do
                  div_ [class_ "code-tag"] "Recommended Pattern"
                  pre_ (code_ (toHtml (icCodeGood c)))
            div_ [class_ "trap-visuals single-view", id_ ("trap-vis-" <> tid)] $ do
              div_ [class_ "trap-svg-box trap-svg-bad", id_ ("trap-bad-" <> tid)] $ do
                div_ [class_ "trap-svg-label"] (toHtml (icLabelA c))
                div_ [class_ "svg-well"] (toHtmlRaw (icSvgA c))
              div_ [class_ "trap-svg-box trap-svg-good hidden-compare", id_ ("trap-good-" <> tid)] $ do
                div_ [class_ "trap-svg-label"] (toHtml (icLabelB c))
                div_ [class_ "svg-well"] (toHtmlRaw (icSvgB c))

buildWidgetMetaValue :: [ProbeRow] -> Value
buildWidgetMetaValue rows =
  object
    [ prId pr
        .= object
          [ "expr" .= prCodeExpr pr
          , "isOverlay" .= prIsOverlay pr
          , "notes" .= prToggles pr
          ]
    | pr <- rows
    ]

buildSvgBanksValue :: Value -> Value -> [ProbeRow] -> TypoSectionData -> Value
buildSvgBanksValue heroBank stateBank rows tsd =
  object $
    [ "hero" .= heroBank
    , "state" .= stateBank
    , "typo" .= tsdBankValue tsd
    ]
      ++ [prId pr .= prBankValue pr | pr <- rows]

buildTrapsValue :: [InteractionCard] -> Value
buildTrapsValue = toJSON

-- | The @aria-label@ attribute, which Lucid does not name.
ariaLabel_ :: Text -> Attributes
ariaLabel_ = makeAttributes "aria-label"

-- | A sidebar navigation list.
navList :: [(Text, Text)] -> Html ()
navList items =
  ul_ [class_ "nav-list"] $
    mapM_ (\(href, navLabel) -> li_ (a_ [href_ href] (toHtml navLabel))) items

-- | A surface-probe legend row.
legendRow :: Text -> Text -> Text -> Html ()
legendRow bg border rowLabel =
  div_ [class_ "legend-row"] $ do
    span_ [class_ "swatch", style_ ("background:" <> bg <> ";border:2px solid " <> border)] mempty
    span_ (toHtml rowLabel)

-- | The fixed sidebar: brand, probe color key, and section/trap links.
sidebarNav :: Html ()
sidebarNav = do
  div_ [class_ "brand-kicker"] "nano-ui-svg Styling Lab"
  h1_ [class_ "brand-title"] "Styling & Surface Glossary"
  p_ [class_ "brand-sub"] $ do
    txt "Toggle any combination of modifiers below. Every preview is a real headless "
    code_ "NanoUI ()"
    txt " frame exported with "
    code_ "frameToSvg"
    txt "."
  div_ [class_ "legend-box"] $ do
    div_ [class_ "nav-group-label", style_ "margin-top:0"] "Surface Probe Color Key"
    legendRow "#0369a1" "#38bdf8" "themeButton (buttonStyle)"
    legendRow "#047857" "#34d399" "themeInput (inputStyle)"
    legendRow "#581c87" "#c084fc" "themePanel (panelStyle)"
    legendRow "#b45309" "#fbbf24" "themePopup (popupStyle)"
    legendRow "#e11d48" "#fb7185" "themeAccent (accentColor)"
  div_ [class_ "nav-group-label"] "Sections"
  navList
    [ ("#sec-states", "I. Interaction States")
    , ("#sec-routing", "II. Surface Routing")
    , ("#sec-matrix", "III. Widget Styles")
    , ("#sec-typo", "IV. Typography")
    , ("#sec-traps", "V. Unexpected Interactions")
    ]
  div_ [class_ "nav-group-label"] "Unexpected Interactions"
  navList trapLinks
  where
    trapLinks =
      [ ("#trap-01", "#01 checkbox 3-Surface + Modal Shift")
      , ("#trap-02", "#02 select vs comboBox Surface")
      , ("#trap-03", "#03 scrollWith grow Loses Well")
      , ("#trap-04", "#04 callout Leaks panelStyle")
      , ("#trap-05", "#05 toggleButton ON Overwrites Style")
      , ("#trap-06", "#06 borderWidth Zero Inset & Sides")
      , ("#trap-07", "#07 (.) Order vs Nested styled")
      , ("#trap-08", "#08 tabs Read themePanel, Not Button")
      ]

-- | The top banner: intro copy and the live composite specimen.
heroSection :: Text -> Html ()
heroSection heroBaseSvg =
  header_ [class_ "hero-banner", id_ "hero-workbench"] $ do
    div_ [class_ "hero-top"] $
      div_ $ do
        h1_ [class_ "hero-title"] $ do
          code_ "nano-ui"
          txt " Styling & Surfaces"
        p_ [class_ "hero-lead"] $ do
          txt "Widgets have no per-widget CSS. A "
          code_ "Theme"
          txt " holds five surface "
          code_ "Style"
          txt " records ("
          code_ "themeButton"
          txt ", "
          code_ "themeInput"
          txt ", "
          code_ "themePanel"
          txt ", "
          code_ "themePopup"
          txt ", "
          code_ "themeFloatingWindow"
          txt "). Toggle styles to see which surface each widget reads."
    div_ [class_ "wb-grid"] $ do
      div_ [class_ "wb-viewport-col"] $ do
        div_ [class_ "wb-viewport-head"] $ do
          span_ [class_ "wb-active-label", id_ "hero-status"] "Composite · defaultTheme (0 modifiers)"
          div_ [class_ "wb-head-actions"] $ do
            miniBtn [onclick_ "setHeroMask(30)"] "All surfaces"
            miniBtn [onclick_ "setHeroMask(0)"] "Reset"
        div_ [class_ "svg-well", id_ "hero-svg-well"] (toHtmlRaw heroBaseSvg)
        div_ [class_ "wb-code-bar"] (code_ [id_ "hero-code-preview"] "compositeDashboardView -- toggle modifiers on the right")
      div_ [class_ "wb-controls-col"] $ do
        div_ [class_ "control-group-label"] "Toggle modifiers:"
        div_ [class_ "toggle-chip-grid", id_ "hero-toggles"] $ mapM_ heroChip heroChips
  where
    heroChips =
      [ (0 :: Int, "light", "Light Theme", "impact-primary", "Base")
      , (1, "btn", "buttonStyle", "impact-primary", "Button")
      , (2, "inp", "inputStyle", "impact-primary", "Input")
      , (3, "pnl", "panelStyle", "impact-primary", "Panel")
      , (4, "acc", "accentColor", "impact-primary", "Accent")
      , (5, "rnd", "rounded 999", "impact-partial", "Partial")
      , (6, "dis", "disabledWhen", "impact-primary", "Dims")
      ]
    heroChip :: (Int, Text, Text, Text, Text) -> Html ()
    heroChip (bit, probe, name, toneCls, tag) =
      styleChip
        [ data_ "hbit" (showT bit)
        , data_ "probe" probe
        , onclick_ ("toggleHeroBit(" <> showT bit <> ")")
        ]
        name
        (Just (toneCls, tag))

-- | The section filter toolbar.
filterBar :: Html ()
filterBar = do
  div_ [class_ "filter-bar", role_ "toolbar", ariaLabel_ "Filter glossary"] $ do
    strong_ [style_ "font-family:var(--font-mono);font-size:12px;margin-right:4px;"] "Filter Sections:"
    filterBtn True "all" "All Specimens"
    filterBtn False "surface-button" "themeButton (buttonStyle)"
    filterBtn False "surface-input" "themeInput (inputStyle)"
    filterBtn False "surface-panel" "themePanel (panelStyle)"
    filterBtn False "typography" "Typography"
    filterBtn False "trap" "8 Unexpected Interactions"
  where
    filterBtn :: Bool -> Text -> Text -> Html ()
    filterBtn active cat btnLabel =
      button_
        [ class_ (if active then "filter-btn active" else "filter-btn")
        , onclick_ ("filterGlossary('" <> cat <> "', this)")
        ]
        (toHtml btnLabel)

-- | Section II: the widget-to-surface routing matrix.
routingTable :: Html ()
routingTable =
  div_ [class_ "routing-table-wrap"] $
    table_ [class_ "routing-table"] $ do
      thead_ (tr_ (mapM_ (\h -> th_ (toHtml h)) routingHeader))
      tbody_ (mapM_ (\cells -> tr_ (mapM_ (td_ . inline) cells)) routingRows)

routingHeader :: [Text]
routingHeader =
  [ "Widget"
  , "Arena NodeType"
  , "Resting Fill / Well"
  , "Border / Outline"
  , "Text / Glyph Color"
  , "Active / Check / Focus Behavior"
  , "Gotchas / Traps"
  ]

-- | One row per widget family; each cell is inline markup.
routingRows :: [[[Snippet]]]
routingRows =
  [ [ [Code "button", Txt ", ", Code "iconButton"]
    , [Code "NodeButton"]
    , [Badge "b-btn" "themeButton", Txt " ", Code "styleBg"]
    , [Badge "b-btn" "themeButton", Txt " ", Code "styleBorder"]
    , [Badge "b-btn" "themeButton", Txt " ", Code "styleFg"]
    , [Txt "Hover: ", Code "styleHoverBg", Txt "; Press: ", Code "styleActiveBg", Txt "; Focus: outer 2px ", Code "themeFocusRing"]
    , [Txt "None"]
    ]
  , [ [Code "toggleButton"]
    , [Code "NodeButton", Txt " (toggle)"]
    , [Txt "OFF: ", Badge "b-btn" "themeButton", Brk, Txt "ON: ", Badge "b-acc" "toneColor"]
    , [Txt "OFF: ", Badge "b-btn" "themeButton", Brk, Txt "ON: ", Badge "b-acc" "toneColor"]
    , [Txt "OFF: ", Badge "b-btn" "themeButton", Brk, Txt "ON: ", Code "readableOn"]
    , [Txt "When ON, overwrites ", Code "styleBg", Txt ", ", Code "styleHoverBg", Txt ", ", Code "styleBorder", Txt ", ", Code "styleFg"]
    , [Link "#trap-05" [Txt "ON state ignores ", Code "buttonStyle", Txt " colors (#05)"]]
    ]
  , [ [Code "checkbox", Txt ", ", Code "radio"]
    , [Code "NodeButton", Txt " (choice)"]
    , [Txt "Unchecked well: ", Badge "b-inp" "themeInput", Txt " ", Code "styleBg"]
    , [Txt "Unchecked ring/box: ", Badge "b-btn" "themeButton", Txt " ", Code "styleBorder"]
    , [Txt "Label: ", Badge "b-btn" "themeButton", Txt " ", Code "styleFg"]
    , [Txt "Checked box/dot: ", Badge "b-acc" "themeAccent", Txt " + ", Code "themeOnAccent"]
    , [Link "#trap-01" [Txt "3-surface split + switches to ", Code "themePopup", Txt " in ", Code "modal", Txt " (#01)"]]
    ]
  , [ [Code "select"]
    , [Code "NodeSelect"]
    , [Badge "b-btn" "themeButton", Txt " ", Code "styleBg"]
    , [Badge "b-btn" "themeButton", Txt " ", Code "styleBorder"]
    , [Badge "b-btn" "themeButton", Txt " ", Code "styleFg", Txt " (text & chevron)"]
    , [Txt "Focused border: ", Badge "b-acc" "themeAccent", Txt "; Open menu: ", Badge "b-pop" "themePopup"]
    , [Link "#trap-02" [Txt "Uses ", Code "themeButton", Txt ", not ", Code "themeInput", Txt " (#02)"]]
    ]
  , [ [Code "comboBox"]
    , [Code "NodeTextInput"]
    , [Badge "b-inp" "themeInput", Txt " ", Code "styleBg"]
    , [Badge "b-inp" "themeInput", Txt " ", Code "styleBorder"]
    , [Txt "Text: ", Badge "b-inp" "themeInput", Txt " ", Code "styleFg", Txt "; Chevron: 45% lerp to bg"]
    , [Txt "Focused border: ", Badge "b-acc" "themeAccent", Txt ", bg: ", Code "styleActiveBg", Txt "; Menu: ", Badge "b-pop" "themePopup"]
    , [Link "#trap-02" [Txt "Paired with ", Code "select", Txt " but on ", Code "themeInput", Txt " (#02)"]]
    ]
  , [ [Code "textInput", Txt ", ", Code "searchInput", Txt ", ", Code "numericInput", Txt ", ", Code "textArea"]
    , [Code "NodeTextInput", Txt " / ", Code "NodeTextArea"]
    , [Badge "b-inp" "themeInput", Txt " ", Code "styleBg"]
    , [Badge "b-inp" "themeInput", Txt " ", Code "styleBorder"]
    , [Badge "b-inp" "themeInput", Txt " ", Code "styleFg", Txt " (placeholder 40% lerp to bg)"]
    , [Txt "Focused: swaps border to ", Badge "b-acc" "themeAccent", Txt " & bg to ", Code "styleActiveBg(themeInput)", Txt " (no outer ring)"]
    , [Txt "None"]
    ]
  , [ [Code "slider"]
    , [Code "NodeSlider"]
    , [Txt "35% lerp from ", Badge "b-inp" "themeInput", Txt " ", Code "styleBg", Txt " to ", Code "styleBorder"]
    , [Badge "b-inp" "themeInput", Txt " ", Code "styleBorder", Txt " (1px fixed)"]
    , [Txt "Thumb fill: ", Code "themeOnAccent"]
    , [Txt "Filled bar: ", Badge "b-acc" "themeAccent", Txt "; Focus: outer ", Code "themeFocusRing"]
    , [Txt "Ignores ", Code "borderWidth", Txt " and ", Code "cornerRadius", Txt " (hardcoded 1px / 3px)"]
    ]
  , [ [Code "panel", Txt ", ", Code "card", Txt ", ", Code "callout"]
    , [Code "NodePanel"]
    , [Badge "b-pnl" "themePanel", Txt " ", Code "styleBg"]
    , [Badge "b-pnl" "themePanel", Txt " ", Code "styleBorder"]
    , [Txt "Inherited by child ", Code "label", Txt "s via ", Badge "b-pnl" "themePanel", Txt " ", Code "styleFg"]
    , [Txt "Clips children to ", Code "borderContentClip"]
    , [Link "#trap-04" [Code "callout", Txt " leaks tinted ", Code "panelStyle", Txt " to child panels (#04)"]]
    ]
  , [ [Code "label", Txt ", ", Code "heading", Txt ", ", Code "kv"]
    , [Code "NodeText"]
    , [Txt "Transparent (except in ", Code "frameToSvg", Txt " or striped tables)"]
    , [Txt "None"]
    , [Badge "b-pnl" "themePanel", Txt " ", Code "styleFg", Txt " (or ", Code "fontColor", Txt " / ", Code "fontTone", Txt ")"]
    , [Code "heading", Txt " is ", Code "fontHeading", Txt ": medium weight in ", Badge "b-pnl" "themePanel", Txt " ", Code "styleFg"]
    , [Txt "None"]
    ]
  , [ [Code "scrollWith"]
    , [Code "NodeScroll"]
    , [Txt "Bounded: ", Badge "b-inp" "themeInput", Txt " ", Code "styleBg", Brk, Code "grow", Txt ": ", Code "pageBackground"]
    , [Txt "Bounded: ", Badge "b-inp" "themeInput", Txt " ", Code "styleBorder", Brk, Code "grow", Txt ": None"]
    , [Txt "Scrollbar blends ", Badge "b-inp" "themeInput", Txt " + ", Code "themeSeparator"]
    , [Txt "Always forces ", Code "styleCornerRadius = 0", Txt " on the scroll well"]
    , [Link "#trap-03" [Code "scrollWith grow", Txt " drops well & border (#03)"]]
    ]
  , [ [Code "tabs", Txt ", ", Code "tabBar"]
    , [Code "NodeButton", Txt " (tab)"]
    , [Badge "b-pnl" "themePanel", Txt " / ", Badge "b-acc" "themeAccent"]
    , [Badge "b-pnl" "themePanel", Txt " (", Code "TabSegmented", Txt " uses ", Badge "b-btn" "themeButton", Txt ")"]
    , [Badge "b-pnl" "themePanel", Txt " ", Code "styleFg", Txt " (inactive dimmed to alpha 175)"]
    , [Txt "Selected indicator: ", Badge "b-acc" "themeAccent"]
    , [Link "#trap-08" [Txt "Ignores ", Code "buttonStyle", Txt " on 3 of 4 tab styles (#08)"]]
    ]
  , [ [Code "table", Txt ", ", Code "simpleTable"]
    , [Code "NodePanel", Txt " + ", Code "NodeButton", Txt " + ", Code "NodeText"]
    , [Txt "Headers: 55% ", Badge "b-pnl" "themePanel", Txt " + ", Badge "b-btn" "themeButton", Txt "; Rows: striped"]
    , [Txt "Outer: ", Badge "b-pnl" "themePanel", Txt "; Dividers: ", Code "themeSeparator"]
    , [ Txt "Cells: ", Badge "b-pnl" "themePanel", Txt " ", Code "styleFg"
      , Txt "; Sorted hdr: ", Badge "b-btn" "themeButton", Txt " ", Code "styleFg"
      , Txt "; Unsorted: ", Code "themeMuted"
      ]
    , [Txt "Selected row blends 25% toward ", Badge "b-acc" "themeAccent"]
    , [Txt "Unsorted headers ignore ", Code "buttonStyle (foreground ...)"]
    ]
  , [ [Code "tree"]
    , [Code "NodeButton", Txt " (row)"]
    , [Txt "Even: 25% ", Code "themeWindow", Txt "; Odd: 35% ", Badge "b-btn" "themeButton", Txt " over ", Badge "b-pnl" "themePanel"]
    , [Txt "None (forced 0)"]
    , [Badge "b-btn" "themeButton", Txt " ", Code "styleFg", Txt " (label & chevron)"]
    , [Txt "Selected row blends 25% toward ", Badge "b-acc" "themeAccent"]
    , [Txt "Row stripes blend 3 surfaces"]
    ]
  , [ [Code "modal", Txt ", ", Code "popup", Txt ", ", Code "menuItem"]
    , [Code "NodeFloatingPanel", Txt " / ", Code "NodeButton"]
    , [Badge "b-pop" "themePopup", Txt " ", Code "styleBg", Txt " (scrim: ", Code "themeOverlayDim", Txt ")"]
    , [Badge "b-pop" "themePopup", Txt " ", Code "styleBorder"]
    , [Badge "b-pop" "themePopup", Txt " ", Code "styleFg"]
    , [Txt "Menu hover falls back to ", Badge "b-btn" "themeButton", Txt " ", Code "styleHoverBg", Txt " when popup ", Code "hoverBg == bg"]
    , [Txt "Inner ", Code "panel", Txt "s inside a modal still read ", Code "themePanel"]
    ]
  , [ [Code "toggleSwitch", Txt ", ", Code "progressBar", Txt ", ", Code "knob", Txt ", ", Code "sparkline"]
    , [Code "NodeDrawing"]
    , [Txt "Controls: ", Badge "b-btn" "themeButton", Txt " ", Code "styleBg", Brk, Code "sparkline", Txt ": ", Badge "b-pnl" "themePanel", Txt " ", Code "styleBg"]
    , [Badge "b-btn" "themeButton", Txt " ", Code "styleBorder", Txt " (1 to 1.5px fixed)"]
    , [Badge "b-btn" "themeButton", Txt " ", Code "styleFg"]
    , [Txt "Active fill/arc/line: ", Badge "b-acc" "themeAccent", Txt " + ", Code "themeOnAccent"]
    , [Txt "Custom canvas paths ignore ", Code "cornerRadius", Txt " & ", Code "borderWidth"]
    ]
  ]

-- | Section III master controls: apply one mask to every widget card at once.
masterBar :: Html ()
masterBar = do
  div_ [class_ "master-bar"] $ do
    div_ [class_ "master-bar-top"] $ do
      span_ [class_ "master-bar-title"] "Apply a style to every widget:"
      div_ [class_ "wb-head-actions"] $ do
        miniBtn [onclick_ "setAllWidgetsMask(30)"] "All surfaces"
        miniBtn [onclick_ "setAllWidgetsMask(6)"] "Button and input"
        miniBtn [onclick_ "setAllWidgetsMask(32)"] "Rounded 999"
        miniBtn [id_ "global-cmp-btn", onclick_ "toggleGlobalCompare()"] "⇄ Compare all"
        miniBtn [onclick_ "setAllWidgetsMask(0)"] "Reset all"
    div_ [class_ "toggle-chip-grid", id_ "master-toggles"] $ mapM_ globalChip globalChips
  where
    globalChips =
      [ (0 :: Int, "light", "defaultLightTheme")
      , (1, "btn", "buttonStyle")
      , (2, "inp", "inputStyle")
      , (3, "pnl", "panelStyle")
      , (4, "acc", "accentColor")
      , (5, "rnd", "rounded 999")
      , (6, "dis", "disabledWhen True")
      ]
    globalChip :: (Int, Text, Text) -> Html ()
    globalChip (bit, probe, name) =
      styleChip
        [ data_ "gbit" (showT bit)
        , data_ "probe" probe
        , onclick_ ("toggleGlobalBit(" <> showT bit <> ")")
        ]
        name
        Nothing

-- | The whole page. Lucid escapes text and attributes; the stylesheet, script,
-- JSON payloads, and rendered SVG documents go in raw because they are not
-- prose to escape.
glossaryPage ::
  Assets ->
  (Text, Value) ->
  (Text, Value) ->
  [ProbeRow] ->
  TypoSectionData ->
  [InteractionCard] ->
  Html ()
glossaryPage assets (heroBaseSvg, heroBankVal) (stateBaseSvg, stateBankVal) probeRows typoData interactions = do
  doctype_
  html_ [lang_ "en"] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      title_ "nano-ui Styling & Surface Glossary"
      style_ (assetsFontCss assets <> "\n" <> assetsCss assets)
    body_ $ do
      div_ [class_ "layout-shell"] $ do
        aside_ [class_ "sidebar"] sidebarNav
        main_ [class_ "main-content"] $ do
          heroSection heroBaseSvg
          filterBar
          section_ [id_ "sec-states"] $ do
            sectionHead [Txt "I. Interaction States"] [Txt "Prerendered"]
            stateStripSection stateBaseSvg
          section_ [id_ "sec-routing"] $ do
            sectionHead [Txt "II. Surface Routing"] [Txt "Which theme slot paints what"]
            routingTable
          section_ [id_ "sec-matrix"] $ do
            sectionHead [Txt "III. Widget Styles"] [Txt "Prerendered"]
            masterBar
            mapM_ probeRowCard probeRows
          section_ [id_ "sec-typo"] $ do
            sectionHead [Txt "IV. Typography"] [Txt "Modifiers and specimens"]
            div_ [class_ "typo-grid"] (typoSection typoData)
          section_ [id_ "sec-traps"] $ do
            sectionHead [Txt "V. Unexpected Interactions"] [Txt "Click to compare"]
            interactionsSection interactions
      script_ banksJs
      script_ (assetsJs assets)
  where
    banksJs =
      "const SVG_BANKS = "
        <> encodeJsonText (buildSvgBanksValue heroBankVal stateBankVal probeRows typoData)
        <> ";\nconst WIDGET_META = "
        <> encodeJsonText (buildWidgetMetaValue probeRows)
        <> ";"

buildHtmlDocument ::
  Assets ->
  (Text, Value) ->
  (Text, Value) ->
  [ProbeRow] ->
  TypoSectionData ->
  [InteractionCard] ->
  Text
buildHtmlDocument assets hero state rows typo interactions =
  TL.toStrict (renderText (glossaryPage assets hero state rows typo interactions))
