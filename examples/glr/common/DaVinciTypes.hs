-----------------------------------------------------------------------------------------
-- Haskell binding for daVinci API
-- 
-- Original version:       Sven Panne  <Sven.Panne@informatik.uni-muenchen.de>  1997/99
-- Adapted to daVinci 2.1: Tim Geisler <Tim.Geisler@informatik.uni-muenchen.de> May 1998
--                         marked all extensions with '(V2.1 API)'
-----------------------------------------------------------------------------------------
-- Some changes to names from daVinci API:
--    foo_bar              => FooBar
--    baz                  => DVBaz   in case of name collision
--    foo x  and    foo    => Foo (Maybe x)
--
-- Note: There are some exceptions to the above rules (but I can't remember...  ;-)

module DaVinciTypes(
   DaVinciCmd(..), GraphCmd(..), MultiCmd(..), MenuCmd(..), FileMenuCmd(..),
   ViewMenuCmd(..), NavigationMenuCmd(..), AbstractionMenuCmd(..), LayoutMenuCmd(..),
   AppMenuCmd(..), SetCmd(..), WindowCmd(..), TclCmd(..), SpecialCmd(..), 
   VisualCmd(..), DragAndDropCmd(..), -- (V2.1 API)

   DaVinciAnswer(..),

   Node(..), Edge(..), Attribute(..),

   NodeUpdate(..), EdgeUpdate(..), AttrChange(..),
   MixedUpdate(..), TypeChange(..), -- (V2.1 API)

   MenuEntry(..), IconEntry(..),
   VisualRule(..), -- (V2.1 API)

   NodeId(..), EdgeId(..), MenuId(..), MenuLabel(..), MenuMne(..),
   MenuAcc(..), IconId(..), Type(..), Filename(..), ContextId(..),
   WindowId(..), -- (V2.1 API)

   Orient(..), Direction(..), Btype(..), MenuMod(..)
   )
where

--- API commands ----------------------------------------------------------

data DaVinciCmd =                   -- Commands of the API (top-level). 
    Graph GraphCmd                     -- Graph category
  | Multi MultiCmd                     -- Multi category
  | Menu MenuCmd                       -- Menu category
  | AppMenu AppMenuCmd                 -- AppMenu category
  | DVSet SetCmd                       -- Set category
  | Window WindowCmd                   -- Window category
  | Tcl TclCmd                         -- Tcl category
  | Special SpecialCmd                 -- Special category
  | DVNothing                          -- No operation, for syncronization.
  | Visual VisualCmd                   -- Visual category (V2.1 API)
  | DragAndDrop DragAndDropCmd	       -- Drag and Drop category (V2.1 API)
  deriving Eq

data GraphCmd =                     -- Send and update graphs
    New [Node]                         -- Send new graph
  | NewPlaced [Node]                   -- Dito, better layout
  | Update [NodeUpdate] [EdgeUpdate]   -- Send graph updates
  | ChangeAttr [AttrChange]            -- Change attributes
  | UpdateAndChangeAttr [NodeUpdate] [EdgeUpdate] [AttrChange]
                                       -- Combination of both
  | UpdateMixed [MixedUpdate]	       -- Send mixed graph updates (V2.1 API)
  | UpdateAndChangeAttrMixed [MixedUpdate] [AttrChange]
                                       -- Combination of both (V2.1 API)
  | ChangeType [TypeChange]            -- Change types (V2.1 API)
  deriving Eq

data MultiCmd =                     -- For multi-graph mode
    NewContext                         -- Open graph context
  | OpenContext ContextId              -- Dito, but ID is given
  | SetContext ContextId               -- Switch to context
  | SetContextWindow ContextId WindowId
                                       -- switch to context and window (V2.1 API)
  deriving Eq

data MenuCmd =                      -- Call functions of menu
    File FileMenuCmd                   -- File menu category
  | View ViewMenuCmd                   -- View menu category
  | Navigation NavigationMenuCmd       -- Navigation menu category
  | Abstraction AbstractionMenuCmd     -- Abstraction menu category
  | Layout LayoutMenuCmd               -- Layout menu category
  deriving Eq

data FileMenuCmd =                  -- File menu functions
    ClearGraph                         -- Clear graph.
  | OpenGraph Filename                 -- Load graph from file
  | OpenGraphPlaced Filename           -- Dito, better layout
  | OpenStatus Filename                -- Load status from file
  | SaveGraph Filename                 -- Save graph as term
  | SaveStatus Filename                -- Save graph as status
  | Print (Maybe Filename)             -- Save as PostScript
  | Close                              -- Close graph window
  | Exit                               -- Exit daVinci
  deriving Eq

data ViewMenuCmd =                  -- View menu functions
    OpenNewView                        -- Open additional view
  | OpenSurveyView                     -- Open survey view
  | FullScale                          -- Set scale to 100%
  | FitScaleToWindow                   -- Set scale to fit
  | Scale (Maybe Int)                  -- Set scale to Int
  | GraphInfo                          -- Open Graph Info dialog
  | DaVinciInfo                        -- Open daVinci Info dialog
  deriving Eq

data NavigationMenuCmd =            -- Navigation menu functions
    SelectParents [NodeId]             -- Select parents of nodes
  | SelectSiblings [NodeId]            -- Select siblings of nodes
  | SelectChilds [NodeId]              -- Select childs of nodes
  | SelectChildren [NodeId]	       -- Select childs of nodes (V2.1 API)
  | Navigator (Maybe (NodeId,Direction,Bool)) -- Navigate in graph
  | Find (Maybe (String,Bool,Bool))    -- Find a node
  deriving Eq

data AbstractionMenuCmd =           -- Abstraction menu functions
    HideSubgraph [NodeId]              -- Hide subgraphs of nodes
  | ShowSubgraph [NodeId]              -- Show subgraphs of nodes
  | RestoreAllSubgraphs                -- Show all hidden subgr
  | HideEdges [NodeId]                 -- Hide edges of nodes
  | ShowEdges [NodeId]                 -- Show edges of nodes
  | RestoreAllEdges                    -- Show all hidden edges
  deriving Eq

data LayoutMenuCmd =                -- Layout menu functions
    ImproveAll                         -- Start layout algorithm
  | ImproveVisible                     -- Dito, only visible nodes
  | CompactAll                         -- Compact graph layout
  | Orientation Orient                 -- Switch orientation
  deriving Eq

data AppMenuCmd =                   -- Create menus/icons
    CreateMenus [MenuEntry]            -- Add menus in Edit
  | CreateIcons [IconEntry]            -- Add icons in icon-bar
  | ActivateMenus [MenuId]             -- Enable menus
  | ActivateIcons [IconId]             -- Enable icons
  | ControlFileEvents                  -- Get events of File menu
  deriving Eq

data SetCmd =                       -- Set options
    LayoutAccuracy Int                 -- Layout algorithm params
  | KeepNodesAtLevels Bool             -- Keep nodes at levels
  | FontSize Int                       -- Node font size
  | GapWidth Int                       -- Min. node distance
  | GapHeight Int                      -- Min. level distance
  | MultiEdgeGap Int                   -- Distance for multi-edges
  | SelfEdgeRadius Int                 -- Distance for self-edges
  | ScrollingOnSelection Bool          -- Auto focusing node
  | AnimationSpeed Int                 -- Speed of animation
  | NoCache Bool                       -- Control pixmap caching. Details
  | RulesFirst Bool		       -- Should rules overlap attributes? (V2.1 API)
  deriving Eq

data WindowCmd =                    -- Control windows
    Title String                       -- Set window title
  | ShowMessage String                 -- Left footer message
  | ShowStatus String                  -- Right footer message
  | Position Int Int                   -- Window origin x/y
  | Size Int Int                       -- Window width/height
  | Raise                              -- Raise window
  | Iconify                            -- Iconify window
  | Deiconify                          -- Deiconify window
  | Activate                           -- Enable interaction
  | Deactivate                         -- Disable interaction
  | FileBrowser Bool String String String String [Btype] Bool
                                       -- Show file browser
  deriving Eq

data TclCmd =                       -- Tcl/Tk interface
    DVEval String                      -- Eval Tcl/Tk script
  | EvalFile Filename                  -- Dito, from file
  deriving Eq

data SpecialCmd =                   -- Special commands
    SelectNodes [NodeId]               -- Select specified nodes
  | SelectEdge EdgeId                  -- Select specified edge
  | FocusNode NodeId                   -- Scroll to specified node
  | FocusNodeAnimated NodeId           -- Dito, with animation
  | ShowUrl String                     -- Display HTML-page
  deriving Eq

data VisualCmd =	            -- Visual commands (V2.1 API)
    NewRules [VisualRule]	       -- Specify new rules
  | AddRules [VisualRule]	       -- Add rules or exchange existing ones
  deriving Eq

data DragAndDropCmd =		    -- Drag and Drop commands (V2.1 API)
    DraggingOn	                       -- Switch dragging on
  | DragAndDropOn	               -- Switch drag&drop on
  | DraggingOff	                       -- Switch drag* off
  | NewNodeAtCoord NodeUpdate	       -- Insert at coordinate
  | NewEdgeAndNodeAtCoord NodeUpdate EdgeUpdate
                                       -- Dito, plus edge where node is the child
  deriving Eq

--- API Answers -----------------------------------------------------------

data DaVinciAnswer =                      -- Answers from the API
    Ok                                    -- Positive confirmer
  | CommunicationError String             -- Negative confirmer
  | NodeSelectionsLabels [NodeId]         -- Labels of sel. nodes
  | NodeDoubleClick                       -- Sel. node double-clicked
  | EdgeSelectionLabel EdgeId             -- Label of sel. edge
  | EdgeSelectionLabels NodeId NodeId     -- Dito, parent/child
  | EdgeDoubleClick                       -- Sel. edge double-clicked
  | MenuSelection MenuId                  -- ID of selected menu
  | IconSelection IconId                  -- ID of selected icon
  | Context ContextId                     -- Other context (graph) 
  | TclAnswer String                      -- Answer from Tcl script
  | BrowserAnswer String String           -- File browser result
  | Disconnect                            -- Termination request
  | Closed                                -- Context (graph) closed
  | Quit                                  -- daVinci terminated
  | PopupSelectionNode NodeId MenuId	  -- Pop-up menu selected. (V2.1 API)
  | PopupSelectionEdge EdgeId MenuId	  -- Pop-up menu selected (V2.1 API)
  | CreateNode				  -- Dragging answer (V2.1 API)
  | CreateNodeAndEdge NodeId		  -- Parent ID of new edge (V2.1 API)
  | CreateEdge NodeId NodeId		  -- Node IDs of new edge (V2.1 API)
  | DropNode ContextId WindowId NodeId ContextId WindowId NodeId
                                          -- Node A dropped on B (V2.1 API)
  | ContextWindow ContextId WindowId      -- Context ID + window ID (V2.1 API)
  | OpenWindow				  -- New window opened (V2.1 API)
  | CloseWindow WindowId		  -- Window closed (V2.1 API)
  deriving Eq

--- Term Representation for Graphs ----------------------------------------

data Node =
    N NodeId Type [Attribute] [Edge]   -- Node with ID/type/attr/childs
  | R NodeId                           -- Reference to a node
  deriving Eq

data Edge = E EdgeId Type [Attribute] Node -- Edges with ID/type/attr/child
  deriving Eq

data Attribute =
    A String String		       -- regular node/edge attributes (key/val)
  | M [MenuEntry]		       -- pop-up menu for node/edge (V2.1 API)
  deriving Eq

--- Graph Updates ---------------------------------------------------------

data NodeUpdate =                      -- Delete or remove nodes
    DeleteNode NodeId
  | NewNode NodeId Type [Attribute]
  deriving Eq

data EdgeUpdate =                      -- Delete or remove edges
    DeleteEdge EdgeId
  | NewEdge EdgeId Type [Attribute] NodeId NodeId
  | NewEdgeBehind EdgeId EdgeId Type [Attribute] NodeId NodeId
  deriving Eq

data MixedUpdate =		       -- Node or Edge update (V2.1)
    NU NodeUpdate			  -- wrapper needed in Haskell
  | EU EdgeUpdate			  -- wrapper needed in Haskell
  deriving Eq

data AttrChange =                      -- Change attributes
    Node NodeId [Attribute]
  | Edge EdgeId [Attribute]
  deriving Eq

data TypeChange =		       -- Change types (V2.1 API)
    NodeType NodeId Type	          -- Label, type
  | EdgeType EdgeId Type		  -- Label, type
  deriving Eq

--- Application Menus and Icons -------------------------------------------

data MenuEntry =                       -- Create Menus
    MenuEntry MenuId MenuLabel
  | MenuEntryMne MenuId MenuLabel MenuMne MenuMod MenuAcc
  | SubmenuEntry MenuId MenuLabel [MenuEntry]
  | SubmenuEntryMne MenuId MenuLabel [MenuEntry] MenuMne
  | BlankMenuEntry
  | MenuEntryDisabled MenuId MenuLabel -- (V2.1 API)
  | SubmenuEntryDisabled MenuId MenuLabel [MenuEntry] -- (V2.1 API)
  deriving Eq

data IconEntry =                       -- Create Icons
    IconEntry IconId Filename String
  | BlankIconEntry
  deriving Eq

--- Visualization Rules (V2.1 API) ---------------------------------------

data VisualRule =		       -- (V2.1 API)
    NR Type [Attribute]                  -- Rules for all nodes of given type
  | ER Type [Attribute]			 -- Rules for all edges of given type
  deriving Eq

--- String Sorts ----------------------------------------------------------

newtype NodeId    = NodeId String    deriving Eq   -- Unique node ID
newtype EdgeId    = EdgeId String    deriving Eq   -- Unique edge ID
newtype MenuId    = MenuId String    deriving Eq   -- Unique menu ID
newtype MenuLabel = MenuLabel String deriving Eq   -- Text of menu entry
newtype MenuMne   = MenuMne String   deriving Eq   -- Motif mnemonic char
newtype MenuAcc   = MenuAcc String   deriving Eq   -- Motif accelerator key
newtype IconId    = IconId String    deriving Eq   -- Unique icon ID
newtype Type      = Type String      deriving Eq   -- Arbitrary type
newtype Filename  = Filename String  deriving Eq   -- Valid Filename
newtype ContextId = ContextId String deriving Eq   -- Context ID
newtype WindowId  = WindowId String  deriving Eq   -- Window ID (V2.1 API)

--- Basic Sorts -----------------------------------------------------------

data Orient      = TopDown | BottomUp | LeftRight | RightLeft deriving Eq
data Direction   = Up | Down | DVLeft | DVRight deriving Eq
data Btype       = Bt String String String deriving Eq
                                           -- Text, pattern and title postfix
data MenuMod     = Alternate | Shift | Control | Meta | None deriving Eq
                                           -- Motif modifier key

---------------------------------------------------------------------------
-- Show instances for daVinci API commands
--
-- Everything would be *much* easier if daVinci allowed spaces in commands...

instance Show DaVinciCmd where
   showsPrec _ (Graph graphCmd)                     = showFunc1  "graph" graphCmd
   showsPrec _ (Multi multiCmd)                     = showFunc1  "multi" multiCmd
   showsPrec _ (Menu menuCmd)                       = showFunc1  "menu" menuCmd
   showsPrec _ (AppMenu appMenuCmd)                 = showFunc1  "app_menu" appMenuCmd
   showsPrec _ (DVSet setCmd)                       = showFunc1  "set" setCmd
   showsPrec _ (Window windowCmd)                   = showFunc1  "window" windowCmd
   showsPrec _ (Tcl tclCmd)                         = showFunc1  "tcl" tclCmd
   showsPrec _ (Special specialCmd)                 = showFunc1  "special" specialCmd
   showsPrec _ DVNothing                            = showString "nothing"
   showsPrec _ (Visual visualCmd)		    = showFunc1  "visual" visualCmd
   showsPrec _ (DragAndDrop dragAndDropCmd)	    = showFunc1  "visual" dragAndDropCmd

instance Show GraphCmd where
   showsPrec _ (New nodes)                          = showFunc1  "new" nodes
   showsPrec _ (NewPlaced nodes)                    = showFunc1  "new_placed" nodes
   showsPrec _ (Update nUpds eUpds)                 = showFunc2  "update" nUpds eUpds
   showsPrec _ (ChangeAttr aChs)                    = showFunc1  "change_attr" aChs
   showsPrec _ (UpdateAndChangeAttr nUpds eUpds aChs)
                                                    = showFunc3  "update_and_change_attr" nUpds eUpds aChs
   showsPrec _ (UpdateMixed mUpds)		    = showFunc1  "update" mUpds
   showsPrec _ (UpdateAndChangeAttrMixed mUpds aChs)= showFunc2  "update_and_change_attr" mUpds aChs
   showsPrec _ (ChangeType tChs)                    = showFunc1  "change_type" tChs

instance Show MultiCmd where
   showsPrec _ NewContext                           = showString "new_context"
   showsPrec _ (OpenContext contextId)              = showFunc1  "open_context" contextId
   showsPrec _ (SetContext contextId)               = showFunc1  "set_context" contextId
   showsPrec _ (SetContextWindow contextId windowId)= showFunc2  "set_context" contextId windowId

instance Show MenuCmd where
   showsPrec _ (File fCmd)                          = showFunc1  "file" fCmd
   showsPrec _ (View vCmd)                          = showFunc1  "view" vCmd
   showsPrec _ (Navigation nCmd)                    = showFunc1  "navigation" nCmd
   showsPrec _ (Abstraction aCmd)                   = showFunc1  "abstraction" aCmd
   showsPrec _ (Layout lCmd)                        = showFunc1  "layout" lCmd

instance Show FileMenuCmd where
   showsPrec _ ClearGraph                           = showString "new"
   showsPrec _ (OpenGraph fname)                    = showFunc1  "open_graph" fname
   showsPrec _ (OpenGraphPlaced fname)              = showFunc1  "open_graph_placed" fname
   showsPrec _ (OpenStatus fname)                   = showFunc1  "open_status" fname
   showsPrec _ (SaveGraph fname)                    = showFunc1  "save_graph" fname
   showsPrec _ (SaveStatus fname)                   = showFunc1  "save_status" fname
   showsPrec _ (Print Nothing)                      = showString "print"
   showsPrec _ (Print (Just fname))                 = showFunc1  "print" fname
   showsPrec _ Close                                = showString "close"
   showsPrec _ Exit                                 = showString "exit"

instance Show ViewMenuCmd where
   showsPrec _ OpenNewView                          = showString "open_new_view"
   showsPrec _ OpenSurveyView                       = showString "open_survey_view"
   showsPrec _ FullScale                            = showString "full_scale"
   showsPrec _ FitScaleToWindow                     = showString "fit_scale_to_window"
   showsPrec _ (Scale Nothing)                      = showString "scale"
   showsPrec _ (Scale (Just scale))                 = showFunc1  "scale" scale
   showsPrec _ GraphInfo                            = showString "graph_info"
   showsPrec _ DaVinciInfo                          = showString "daVinci_info"

instance Show NavigationMenuCmd where
   showsPrec _ (SelectParents nodeIds)              = showFunc1  "select_parents" nodeIds
   showsPrec _ (SelectSiblings nodeIds)             = showFunc1  "select_siblings" nodeIds
    -- TODO: change 'childs' to 'children'. But then it's no longer V2.0.x compatible ...
   showsPrec _ (SelectChilds nodeIds)               = showFunc1  "select_childs" nodeIds
   showsPrec _ (SelectChildren nodeIds)             = showFunc1  "select_childs" nodeIds
   showsPrec _ (Navigator Nothing)                  = showString "navigator"
   showsPrec _ (Navigator (Just (nodeId,dir,flag))) = showFunc3  "navigator" nodeId dir flag
   showsPrec _ (Find Nothing)                       = showString "find"
   showsPrec _ (Find (Just (txt,cas,exact)))        = showFunc3  "find" txt cas exact

instance Show AbstractionMenuCmd where
   showsPrec _ (HideSubgraph nodeIds)               = showFunc1  "hide_subgraph" nodeIds
   showsPrec _ (ShowSubgraph nodeIds)               = showFunc1  "show_subgraph" nodeIds
   showsPrec _ RestoreAllSubgraphs                  = showString "restore_all_subgraphs"
   showsPrec _ (HideEdges nodeIds)                  = showFunc1  "hide_edges" nodeIds
   showsPrec _ (ShowEdges nodeIds)                  = showFunc1  "show_edges" nodeIds
   showsPrec _ RestoreAllEdges                      = showString "restore_all_edges"

instance Show LayoutMenuCmd where
   showsPrec _ ImproveAll                           = showString "improve_all"
   showsPrec _ ImproveVisible                       = showString "improve_visible"
   showsPrec _ CompactAll                           = showString "compact_all"
   showsPrec _ (Orientation orient)                 = showFunc1  "orientation" orient

instance Show AppMenuCmd where
   showsPrec _ (CreateMenus menuEntries)            = showFunc1  "create_menus" menuEntries
   showsPrec _ (CreateIcons iconEntries)            = showFunc1  "create_icons" iconEntries
   showsPrec _ (ActivateMenus menuIds)              = showFunc1  "activate_menus" menuIds
   showsPrec _ (ActivateIcons iconIds)              = showFunc1  "activate_icons" iconIds
   showsPrec _ ControlFileEvents                    = showString "control_file_events"

instance Show SetCmd where
   showsPrec _ (LayoutAccuracy x)                   = showFunc1  "layout_accuracy" x
   showsPrec _ (KeepNodesAtLevels x)                = showBoolFunc "keep_nodes_at_levels" x
   showsPrec _ (FontSize x)                         = showFunc1  "font_size" x
   showsPrec _ (GapWidth x)                         = showFunc1  "gap_width" x
   showsPrec _ (GapHeight x)                        = showFunc1  "gap_height" x
   showsPrec _ (MultiEdgeGap x)                     = showFunc1  "multi_edge_gap" x
   showsPrec _ (SelfEdgeRadius x)                   = showFunc1  "self_edge_radius" x
   showsPrec _ (ScrollingOnSelection x)             = showBoolFunc "scrolling_on_selection" x
   showsPrec _ (AnimationSpeed x)                   = showFunc1  "animation_speed" x
   showsPrec _ (NoCache x)                          = showBoolFunc "no_cache" x
   showsPrec _ (RulesFirst x)			    = showBoolFunc "rules_first" x

instance Show WindowCmd where
   showsPrec _ (Title str)                          = showFunc1  "title" str
   showsPrec _ (ShowMessage str)                    = showFunc1  "show_message" str
   showsPrec _ (ShowStatus str)                     = showFunc1  "show_status" str
   showsPrec _ (Position x y)                       = showFunc2  "position" x y
   showsPrec _ (Size w h)                           = showFunc2  "size" w h
   showsPrec _ Raise                                = showString "raise"
   showsPrec _ Iconify                              = showString "iconify"
   showsPrec _ Deiconify                            = showString "deiconify"
   showsPrec _ Activate                             = showString "activate"
   showsPrec _ Deactivate                           = showString "deactivate"
   showsPrec _ (FileBrowser open title btn dir file tps hid)
                                                    = showFunc7 "file_browser" open title btn dir file tps hid

instance Show TclCmd where
   showsPrec _ (DVEval str)                         = showFunc1  "eval" str
   showsPrec _ (EvalFile fname)                     = showFunc1  "eval_file" fname

instance Show SpecialCmd where
   showsPrec _ (SelectNodes nodes)                  = showFunc1  "select_nodes" nodes
   showsPrec _ (SelectEdge edges)                   = showFunc1  "select_edges" edges
   showsPrec _ (FocusNode nodeIds)                  = showFunc1  "focus_node" nodeIds
   showsPrec _ (FocusNodeAnimated nodeIds)          = showFunc1  "focus_node_animated" nodeIds
   showsPrec _ (ShowUrl url)                        = showFunc1  "show_url" url

instance Show VisualCmd where
   showsPrec _ (NewRules visualRules)		    = showFunc1  "new_rules" visualRules
   showsPrec _ (AddRules visualRules)		    = showFunc1  "add_rules" visualRules

instance Show DragAndDropCmd where
   showsPrec _ DraggingOn			    = showString "dragging_on"
   showsPrec _ DragAndDropOn			    = showString "drag_and_drop_on"
   showsPrec _ DraggingOff 			    = showString "dragging_off"
   showsPrec _ (NewNodeAtCoord nUpd)		    = showFunc1  "new_node_at_coord" nUpd
   showsPrec _ (NewEdgeAndNodeAtCoord nUpd eUpd)    = showFunc2  "new_edge_and_node_at_coord" nUpd eUpd
---------------------------------------------------------------------------

instance Show DaVinciAnswer where
   showsPrec _ Ok                                   = showString "ok"
   showsPrec _ (CommunicationError msg)             = showFunc1  "communication_error" msg
   showsPrec _ (NodeSelectionsLabels nodeIds)       = showFunc1  "node_selections_labels" nodeIds
   showsPrec _ NodeDoubleClick                      = showString "node_double_click"
   showsPrec _ (EdgeSelectionLabel edgeId)          = showFunc1  "edge_selection_label" edgeId
   showsPrec _ (EdgeSelectionLabels parent child)   = showFunc2  "edge_selection_labels" parent child
   showsPrec _ EdgeDoubleClick                      = showString "edge_double_click"
   showsPrec _ (MenuSelection menuId)               = showFunc1  "menu_selection" menuId
   showsPrec _ (IconSelection iconId)               = showFunc1  "icon_selection" iconId
   showsPrec _ (Context contextId)                  = showFunc1  "context" contextId
   showsPrec _ (TclAnswer retVal)                   = showFunc1  "tcl_answer" retVal
   showsPrec _ (BrowserAnswer file typ)             = showFunc2  "browser_answer" file typ
   showsPrec _ Disconnect                           = showString "disconnect"
   showsPrec _ Closed                               = showString "closed"
   showsPrec _ Quit                                 = showString "quit"
   showsPrec _ (PopupSelectionNode nId mId)	    = showFunc2	 "popup_selection_node" nId mId
   showsPrec _ (PopupSelectionEdge eId mId)	    = showFunc2	 "popup_selection_edge" eId mId
   showsPrec _ CreateNode			    = showString "create_node"
   showsPrec _ (CreateNodeAndEdge nId)		    = showFunc1	 "create_node_and_edge" nId
   showsPrec _ (CreateEdge nId1 nId2)		    = showFunc2	 "create_edge" nId1 nId2
   showsPrec _ (DropNode cId1 wId1 nId1 cId2 wId2 nId2) = showFunc6 "drop_node" cId1 wId1 nId1 cId2 wId2 nId2
   showsPrec _ (ContextWindow cId wId)		    = showFunc2	 "context_window" cId wId
   showsPrec _ OpenWindow			    = showString "open_window"
   showsPrec _ (CloseWindow wId)		    = showFunc1	 "close_window" wId

instance Read DaVinciAnswer where
   readsPrec _ r =
      [ (Ok,                                        s) | ("ok",                     s) <- lexR       ] ++
      [ (CommunicationError m,                      t) | ("communication_error",    s) <- lexR ,
                                                         ([m],                      t) <- readArgs s ] ++
      [ (NodeSelectionsLabels (map NodeId n),       t) | ("node_selections_labels", s) <- lexR ,
                                                         (n,                        t) <- readStrs s ] ++
      [ (NodeDoubleClick,                           s) | ("node_double_click",      s) <- lexR       ] ++
      [ (EdgeSelectionLabel (EdgeId e),             t) | ("edge_selection_label",   s) <- lexR ,
                                                         ([e],                      t) <- readArgs s ] ++
      [ (EdgeSelectionLabels (NodeId p) (NodeId c), t) | ("edge_selection_labels",  s) <- lexR ,
                                                         ([p,c],                    t) <- readArgs s ] ++
      [ (EdgeDoubleClick,                           s) | ("edge_double_click",      s) <- lexR       ] ++
      [ (MenuSelection (MenuId m),                  t) | ("menu_selection",         s) <- lexR ,
                                                         ([m],                      t) <- readArgs s ] ++
      [ (IconSelection (IconId i),                  t) | ("icon_selection",         s) <- lexR ,
                                                         ([i],                      t) <- readArgs s ] ++
      [ (Context (ContextId c),                     t) | ("context",                s) <- lexR ,
                                                         ([c],                      t) <- readArgs s ] ++
      [ (TclAnswer a,                               t) | ("tcl_answer",             s) <- lexR ,
                                                         ([a],                      t) <- readArgs s ] ++
      [ (BrowserAnswer f y,                         t) | ("browser_answer",         s) <- lexR ,
                                                         ([f,y],                    t) <- readArgs s ] ++
      [ (Disconnect,                                s) | ("disconnect",             s) <- lexR       ] ++
      [ (Closed,                                    s) | ("closed",                 s) <- lexR       ] ++
      [ (Quit,                                      s) | ("quit",                   s) <- lexR       ] ++
      [ (PopupSelectionNode (NodeId n) (MenuId m),  t) | ("popup_selection_node",   s) <- lexR ,
							 ([n,m],		    t) <- readArgs s ] ++
      [ (PopupSelectionEdge (EdgeId e) (MenuId m),  t) | ("popup_selection_edge",   s) <- lexR ,
							 ([e,m],		    t) <- readArgs s ] ++
      [ (CreateNode,				    s) | ("create_node",            s) <- lexR       ] ++
      [ (CreateNodeAndEdge (NodeId n),		    t) | ("create_node_and_edge",   s) <- lexR ,
							 ([n],			    t) <- readArgs s ] ++
      [ (CreateEdge (NodeId n1) (NodeId n2),	    t) | ("create_edge",	    s) <- lexR ,
							 ([n1, n2],		    t) <- readArgs s ] ++
      [ (DropNode (ContextId c1) (WindowId w1) (NodeId n1) (ContextId c2) (WindowId w2) (NodeId n2), t) 
						       | ("drop_node",		    s) <- lexR ,
							 ([c1,w1,n1,c2,w2,n2],	    t) <- readArgs s ] ++
      [ (ContextWindow (ContextId c) (WindowId w), t)| ("context_window",	    s) <- lexR ,
							 ([c,w],		    t) <- readArgs s ] ++
      [ (OpenWindow,				    s) | ("open_window",	    s) <- lexR       ] ++
      [ (CloseWindow (WindowId w),		    t) | ("close_window",	    s) <- lexR ,
							 ([w],			    t) <- readArgs s ]

         where lexR = lex r

               readArgs :: ReadS [String]
               readArgs s = [ (x:xs, v) | ("(", t) <- lex s,
                                          (x,   u) <- reads t,
                                          (xs,  v) <- readArgs2 u ]

               readArgs2 :: ReadS [String]
               readArgs2 s = [ ([],   t) | (")",t) <- lex s ] ++
                             [ (x:xs, v) | (",",t) <- lex s,
                                           (x,  u) <- reads t,
                                           (xs, v) <- readArgs2 u ]

               readStrs :: ReadS [String]
               readStrs = reads

---------------------------------------------------------------------------

instance Show Node where
   showsPrec _ (N nodeId typ attrs edges) = showLabeled nodeId (showFunc3 "n" typ attrs edges)
   showsPrec _ (R nodeId)                 = showFunc1 "r" nodeId
   showList = showLst

instance Show Edge where
   showsPrec _ (E edgeId typ attrs node) = showLabeled edgeId (showFunc3 "e" typ attrs node)
   showList = showLst

instance Show Attribute where
   showsPrec _ (A key value)   = showFunc2 "a" key value
   showsPrec _ (M menuEntries) = showFunc1 "m" menuEntries
   showList = showLst

instance Show NodeUpdate where
   showsPrec _ (DeleteNode nodeId)        = showFunc1 "delete_node" nodeId
   showsPrec _ (NewNode nodeId typ attrs) = showFunc3 "new_node" nodeId typ attrs
   showList = showLst

instance Show EdgeUpdate where
   showsPrec _ (DeleteEdge edgeId) = showFunc1 "delete_edge" edgeId
   showsPrec _ (NewEdge edgeId typ attrs nodeId1 nodeId2) = showFunc5 "new_edge" edgeId typ attrs nodeId1 nodeId2
   showsPrec _ (NewEdgeBehind edgeId1 edgeId2 typ attrs nodeId1 nodeId2) = showFunc6 "new_edge_behind" edgeId1 edgeId2 typ attrs nodeId1 nodeId2
   showList = showLst

instance Show MixedUpdate where
   showsPrec _ (NU nUpd) = shows nUpd
   showsPrec _ (EU eUpd) = shows eUpd
   showList = showLst

instance Show AttrChange where
   showsPrec _ (Node nodeId attrs) = showFunc2 "node" nodeId attrs
   showsPrec _ (Edge edgeId attrs) = showFunc2 "edge" edgeId attrs
   showList = showLst

instance Show TypeChange where
   showsPrec _ (NodeType nodeId typ) = showFunc2 "node" nodeId typ
   showsPrec _ (EdgeType edgeId typ) = showFunc2 "edge" edgeId typ
   showList = showLst

---------------------------------------------------------------------------

instance Show MenuEntry where
   showsPrec _ (MenuEntry menuId menuLabel) = showFunc2 "menu_entry" menuId menuLabel
   showsPrec _ (MenuEntryMne menuId menuLabel menuMne menuMod menuAcc) = showFunc5 "menu_entry_mne" menuId menuLabel menuMne menuMod menuAcc
   showsPrec _ (SubmenuEntry menuId menuLabel menuEntries) = showFunc3 "submenu_entry" menuId menuLabel menuEntries
   showsPrec _ (SubmenuEntryMne menuId menuLabel menuEntries menuMne) = showFunc4 "submenu_entry_mne" menuId menuLabel menuEntries menuMne
   showsPrec _ BlankMenuEntry = showString "blank"
   showsPrec _ (MenuEntryDisabled menuId menuLabel) = showFunc2 "menu_entry_disabled" menuId menuLabel
   showsPrec _ (SubmenuEntryDisabled menuId menuLabel menuEntries) = showFunc3 "submenu_entry_disabled" menuId menuLabel menuEntries

instance Show IconEntry where
   showsPrec _ (IconEntry iconId filename descr) = showFunc3 "icon_entry" iconId filename descr
   showsPrec _ BlankIconEntry                    = showString "blank"

---------------------------------------------------------------------------

instance Show VisualRule where
   showsPrec _ (NR typ attrs) = showFunc2 "nr" typ attrs
   showsPrec _ (ER typ attrs) = showFunc2 "er" typ attrs
   showList = showLst

---------------------------------------------------------------------------

instance Show NodeId where
   showsPrec _ (NodeId s) = shows s
   showList = showLst

instance Show EdgeId where
   showsPrec _ (EdgeId s) = shows s
   showList = showLst

instance Show MenuId where
   showsPrec _ (MenuId s) = shows s
   showList = showLst

instance Show MenuLabel where
   showsPrec _ (MenuLabel s) = shows s
   showList = showLst

instance Show MenuMne where
   showsPrec _ (MenuMne s) = shows s
   showList = showLst

instance Show MenuAcc where
   showsPrec _ (MenuAcc s) = shows s
   showList = showLst

instance Show IconId where
   showsPrec _ (IconId s) = shows s
   showList = showLst

instance Show Type where
   showsPrec _ (Type s) = shows s
   showList = showLst

instance Show Filename where
   showsPrec _ (Filename s) = shows s
   showList = showLst

instance Show ContextId where
   showsPrec _ (ContextId s) = shows s
   showList = showLst

instance Show WindowId where
   showsPrec _ (WindowId s) = shows s
   showList = showLst

---------------------------------------------------------------------------

instance Show Orient where
   showsPrec _ TopDown   = showString "top_down"
   showsPrec _ BottomUp  = showString "bottom_up"
   showsPrec _ LeftRight = showString "left_right"
   showsPrec _ RightLeft = showString "right_left"

instance Show Direction where
   showsPrec _ Up      = showString "up"
   showsPrec _ Down    = showString "down"
   showsPrec _ DVLeft  = showString "left"
   showsPrec _ DVRight = showString "right"

instance Show Btype where
   showsPrec _ (Bt txt pat post) = showFunc3 "bt" txt pat post

instance Show MenuMod where
   showsPrec _ Alternate = showString "alt"
   showsPrec _ Shift     = showString "shift"
   showsPrec _ Control   = showString "control"
   showsPrec _ Meta      = showString "meta"
   showsPrec _ None      = showString "none"

---------------------------------------------------------------------------

showFunc1 :: Show a => String -> a -> ShowS
showFunc1 funcName arg1 =
   showString funcName . showParen True (shows arg1)

showFunc2 :: (Show a,Show b) => String -> a -> b -> ShowS
showFunc2 funcName arg1 arg2 =
   showString funcName . showParen True (shows arg1 . showChar ',' .
                                         shows arg2)

showFunc3 :: (Show a,Show b,Show c) => String -> a -> b -> c -> ShowS
showFunc3 funcName arg1 arg2 arg3 =
   showString funcName . showParen True (shows arg1 . showChar ',' .
                                         shows arg2 . showChar ',' .
                                         shows arg3)

showFunc4 :: (Show a,Show b,Show c,Show d) => String -> a -> b -> c -> d -> ShowS
showFunc4 funcName arg1 arg2 arg3 arg4 =
   showString funcName . showParen True (shows arg1 . showChar ',' .
                                         shows arg2 . showChar ',' .
                                         shows arg3 . showChar ',' .
                                         shows arg4)

showFunc5 :: (Show a,Show b,Show c,Show d,Show e) => String -> a -> b -> c -> d -> e -> ShowS
showFunc5 funcName arg1 arg2 arg3 arg4 arg5 =
   showString funcName . showParen True (shows arg1 . showChar ',' .
                                         shows arg2 . showChar ',' .
                                         shows arg3 . showChar ',' .
                                         shows arg4 . showChar ',' .
                                         shows arg5)

showFunc6 :: (Show a,Show b,Show c,Show d,Show e,Show f) => String -> a -> b -> c -> d -> e -> f -> ShowS
showFunc6 funcName arg1 arg2 arg3 arg4 arg5 arg6 =
   showString funcName . showParen True (shows arg1 . showChar ',' .
                                         shows arg2 . showChar ',' .
                                         shows arg3 . showChar ',' .
                                         shows arg4 . showChar ',' .
                                         shows arg5 . showChar ',' .
                                         shows arg6)

showFunc7 :: (Show a,Show b,Show c,Show d,Show e,Show f,Show g) => String -> a -> b -> c -> d -> e -> f -> g -> ShowS
showFunc7 funcName arg1 arg2 arg3 arg4 arg5 arg6 arg7 =
   showString funcName . showParen True (shows arg1 . showChar ',' .
                                         shows arg2 . showChar ',' .
                                         shows arg3 . showChar ',' .
                                         shows arg4 . showChar ',' .
                                         shows arg5 . showChar ',' .
                                         shows arg6 . showChar ',' .
                                         shows arg7)

showLabeled :: Show a => a -> ShowS -> ShowS
showLabeled iD arg = showChar 'l' . showParen True (shows iD . showChar ',' . arg)

showLst :: Show a => [a] -> ShowS
showLst []       = showString "[]"
showLst (x:xs)   = showChar '[' . shows x . showl xs
                   where showl []     = showChar ']'
                         showl (y:ys) = showChar ',' . shows y . showl ys

showBoolFunc :: String -> Bool -> ShowS
showBoolFunc funcName flag =
   showString funcName . showParen True (showString (if flag then "true" else "false"))
