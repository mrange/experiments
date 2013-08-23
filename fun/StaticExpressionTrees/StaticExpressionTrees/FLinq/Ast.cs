// ----------------------------------------------------------------------------------------------
// Copyright (c) Mårten Rånge.
// ----------------------------------------------------------------------------------------------
// This source code is subject to terms and conditions of the Microsoft Public License. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// If you cannot locate the  Microsoft Public License, please send an email to 
// dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
//  by the terms of the Microsoft Public License.
// ----------------------------------------------------------------------------------------------
// You must not remove this notice, or any other, from this software.
// ----------------------------------------------------------------------------------------------

using System;
using System.Text;

namespace StaticExpressionTrees.FLinq
{
    abstract partial class Ast
    {
        public AstType      Type    ;

        public override string ToString()
        {
            var sb = new StringBuilder();
            ToString (0, sb);
            return sb.ToString();
        }

        internal abstract void ToString (int indent, StringBuilder sb);

        public abstract Type ResultType {get;}
    }

    abstract partial class Ast<TResult> : Ast
    {
        public override Type ResultType
        {
            get { return typeof(TResult); }
        }

    }

    partial class BinaryOperation<TResult> : Ast<TResult>
    {
        public Ast        Left        ;
        public Ast        Right       ;

        internal override void ToString(int indent, StringBuilder sb)
        {
            sb.Append(' ', indent);
            sb.Append('(');
            sb.AppendLine();
            if (Left != null)
            {
                Left.ToString(indent + 2, sb);
            }
            sb.Append(' ', indent);
            sb.Append(')');
            sb.AppendLine();

            sb.Append(' ', indent);
            sb.Append(Type);
            sb.AppendLine();

            sb.Append(' ', indent);
            sb.Append('(');
            sb.AppendLine();
            if (Right != null)
            {
                Right.ToString(indent + 2, sb);
            }
            sb.Append(' ', indent);
            sb.Append(')');
            sb.AppendLine();

        }
    }

    partial class ComputeOperation<T> : BinaryOperation<T>
    {
    }

    partial class CompareOperation : BinaryOperation<bool>
    {


    }

    partial class LogicalOperation : BinaryOperation<bool>
    {

    }

    partial class ValueOperation<T> : Ast<T>
    {
        public T Value;

        public static implicit operator ValueOperation<T>(T v)
        {
            return new ValueOperation<T> {Value = v};
        }

        internal override void ToString(int indent, StringBuilder sb)
        {
            sb.Append(' ', indent);
            sb.Append(Value);
            sb.AppendLine();
        }
    }

    partial class ColumnOperation<T> : Ast<T>
    {
        public string ColumnId;

        internal override void ToString(int indent, StringBuilder sb)
        {
            sb.Append(' ', indent);
            sb.Append("ColumnId=");
            sb.Append(ColumnId ?? "");
            sb.AppendLine();
        }
    }
}
