# Copyright (c) Mårten Rånge. All rights reserved.
# Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.
require 'carnelian/executor'

def attribute(name, key, type)
  {
    name:           name  ,
    key:            key   ,
    type:           type  ,
  }
end

def event(name, key, type)
  {
    name:           name  ,
    key:            key   ,
    type:           type  ,
  }
end

def string(name, key)
  attribute(name, key, "string")
end

def bool(name, key)
  attribute(name, key, "bool")
end

def onstring(name, key)
  event(name, key, "string")
end

def onbool(name, key)
  event(name, key, "bool")
end

def onunit(name, key)
  event(name, key, "Unit")
end

def node(name, tag, attributes)
  {
    name:       name        ,
    tag:        tag         ,
    base:       "Node"      ,
    attributes: attributes  ,
    isContent:  false       ,
  }
end

def contentNode(name, tag, attributes)
  {
    name:       name          ,
    tag:        tag           ,
    base:       "ContentNode" ,
    attributes: attributes    ,
    isContent:  true          ,
  }
end

$types  =
  [
    {
      name:   "Node"                                    ,
      attributes:
        [
          string("AccessKey"        , "accesskey"       ),
          string("Class"            , "class"           ),
          bool(  "ContentEditable"  , "contenteditable" ),
          string("ContextMenu"      , "contextmenu"     ),
          string("Dir"              , "dir"             ),
          bool(  "Draggable"        , "draggable"       ),
          bool(  "DropZone"         , "dropzone"        ),
          bool(  "Hidden"           , "hidden"          ),
          string("Id"               , "id"              ),
          string("Lang"             , "lang"            ),
          bool(  "SpellCheck"       , "spellcheck"      ),
          string("Style"            , "style"           ),
          string("TabIndex"         , "tabindex"        ),
          string("Title"            , "title"           ),
          bool(  "Translate"        , "translate"       ),
        ]                                               ,
        events:
        [
          onunit(   "OnBlur"        , "onblur"          ),
          onstring( "OnChange"      , "onchange"        ),
          onunit(   "OnContextMenu" , "oncontextmenu"   ),
          onunit(   "OnFocus"       , "onfocus"         ),
          onunit(   "OnInput"       , "oninput"         ),
          onunit(   "OnInvalid"     , "oninvalid"       ),
          onunit(   "OnReset"       , "onreset"         ),
          onunit(   "OnSearch"      , "onsearch"        ),
          onunit(   "OnSelect"      , "onselect"        ),
          onunit(   "OnSubmit"      , "onsubmit"        ),
        ]                                               ,
    }                                                   ,
    node(       "Br"    , "br"    , [])                 ,
    {
      name:   "ContentNode"                             ,
      base:   "Node"                                    ,
    }                                                   ,
    contentNode("Button", "button", [])                 ,
    contentNode("Div"   , "div"   , [])                 ,
    node(
      "Img"                                             ,
      "img"                                             ,
      [
        string("Alt"              , "alt"               ),
        string("CrossOrigin"      , "crossorigin"       ),
        bool(  "IsMap"            , "ismap"             ),
        string("LongDesc"         , "longdesc"          ),
        string("Sizes"            , "sizes"             ),
        string("Src"              , "src"               ),
        string("SrcSet"           , "srcset"            ),
        string("UseMap"           , "usemap"            ),
        string("Vspace"           , "vspace"            ),
        string("Width"            , "width"             ),
      ]                                                 ,
    )                                                   ,
    contentNode("H1"    , "h1"    , [])                 ,
    contentNode("H2"    , "h2"    , [])                 ,
    contentNode("H3"    , "h3"    , [])                 ,
    contentNode("H4"    , "h4"    , [])                 ,
    contentNode("H5"    , "h5"    , [])                 ,
    node(       "Hr"    , "hr"    , [])                 ,
    node(
      "Input"                                           ,
      "input"                                           ,
      [
        string("Placeholder"      , "placeholder"       ),
        string("Value"            , "value"             ),
      ]                                                 ,
    )                                                   ,
    contentNode("P"     , "p"     , [])                 ,
    contentNode("Span"  , "span"  , [])                 ,
  ]

CarnelianExecutor.execute_metaprogram_to_file "Generated_Flazor.Elmish.mp", "Generated_Flazor.Elmish.cs"

$overloads = 10

CarnelianExecutor.execute_metaprogram_to_file "Generated_Flazor.Formlets.mp", "Generated_Flazor.Formlets.cs"
