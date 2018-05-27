@@@ metaprogram
@@@ extension cs
// Copyright (c) Mårten Rånge. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

namespace Flazor.Elmish
{
  using System;

  public static partial class Hierarchy
  {
@@> for type in $types
@@>   typeName = type.get :name
@@>   typeBase = type.get :base, 'object'
    public partial class @@=typeName=@@ : @@=typeBase=@@
    {
    }

@@> end
  }

  public static partial class Attributes
  {
@@> for type in $types
@@>   typeName  = type.get :name
@@>   typeBase  = type.get :base, 'object'
    public partial class @@=typeName=@@ : @@=typeBase=@@
    {
@@>   for attribute in type.get :attributes, []
@@>     attributeName = attribute.get :name
@@>     attributeKey  = attribute.get :key
@@>     attributeType = attribute.get :type
      public static readonly IAttribute<Hierarchy.@@=typeName=@@, @@=attributeType=@@> @@=attributeName=@@ = new Attribute<Hierarchy.@@=typeName=@@, @@=attributeType=@@>("@@=attributeKey=@@");
@@>   end
    }

@@> end
  }

  public static partial class Events
  {
@@> for type in $types
@@>   typeName  = type.get :name
@@>   typeBase  = type.get :base, 'object'
    public partial class @@=typeName=@@ : @@=typeBase=@@
    {
@@>   for event in type.get :events, []
@@>     eventName = event.get :name
@@>     eventKey  = event.get :key
@@>     eventType = event.get :type
      public static readonly IEvent<Hierarchy.@@=typeName=@@, @@=eventType=@@> @@=eventName=@@ = new Event<Hierarchy.@@=typeName=@@, @@=eventType=@@>("@@=eventKey=@@");
@@>   end
    }

@@> end
  }

  public static partial class Views<TMessage>
  {
@@> for type in $types
@@>   typeName      = type.get :name
@@>   typeBase      = type.get :base      , 'object'
@@>   typeTag       = type.get :tag       , ''
@@>   typeIsContent = type.get :isContent
    // @@=typeName=@@
@@>   for attribute in type.get :attributes, []
@@>     attributeName = attribute.get :name
@@>     attributeKey  = attribute.get :key
@@>     attributeType = attribute.get :type
    public static ISetValue<TMessage, Hierarchy.@@=typeName=@@, @@=attributeType=@@> @@=attributeName=@@(@@=attributeType=@@ v) => Set_@@=attributeType=@@(Attributes.@@=typeName=@@.@@=attributeName=@@, v);
@@>   end
@@>   for event in type.get :events, []
@@>     eventName = event.get :name
@@>     eventKey  = event.get :key
@@>     eventType = event.get :type
    public static IReceiveValue<TMessage, Hierarchy.@@=typeName=@@, @@=eventType=@@> @@=eventName=@@(Action<@@=eventType=@@> r) => Receive_@@=eventType=@@(Events.@@=typeName=@@.@@=eventName=@@, r);
@@>   end
@@> if typeTag != ''
@@>   if typeIsContent
    public static ContainerView<TMessage> @@=typeName=@@(params IValue<TMessage, Hierarchy.@@=typeName=@@>[] values) => Create_ContainerView<Hierarchy.@@=typeName=@@>("@@=typeTag=@@", values);
@@>   else
    public static IView<TMessage> @@=typeName=@@(params IValue<TMessage, Hierarchy.@@=typeName=@@>[] values) => Create_View<Hierarchy.@@=typeName=@@>("@@=typeTag=@@", values);
@@>   end
@@> end

@@> end
  }
}

@@+ class Hash
@@+     def get (key, defaultValue=nil)
@@+         value = self[key]
@@+         return value unless value == nil
@@+         return defaultValue || ("KEY_'%s'_NOT_FOUND" % key.to_s)
@@+     end
@@+ end

