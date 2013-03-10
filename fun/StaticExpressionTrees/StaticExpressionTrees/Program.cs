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

namespace StaticExpressionTrees
{
    abstract class Ast
    {
        public AstType      Type    ;

        public override string ToString()
        {
            var sb = new StringBuilder();
            ToString (0, sb);
            return sb.ToString();
        }

        public abstract void ToString (int indent, StringBuilder sb);

    }

    partial class BinaryOperation : Ast
    {
        public Ast              Left        ;
        public Ast              Right       ;

        public override void ToString(int indent, StringBuilder sb)
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

    partial class ComputeOperation : BinaryOperation
    {
    }

    partial class CompareOperation : BinaryOperation
    {


    }

    partial class LogicalOperation : BinaryOperation
    {

    }

    partial class ValueOperation : Ast
    {
        public object Value;

        public static implicit operator ValueOperation(int v)
        {
            return new ValueOperation {Value = v};
        }

        public override void ToString(int indent, StringBuilder sb)
        {
            sb.Append(' ', indent);
            sb.Append(Value);
            sb.AppendLine();
        }
    }

    partial class ColumnOperation : Ast
    {
        public string ColumnId;
        public override void ToString(int indent, StringBuilder sb)
        {
            sb.Append(' ', indent);
            sb.Append("ColumnId=");
            sb.Append(ColumnId ?? "");
            sb.AppendLine();
        }
    }


    class Program
    {
        static void Main(string[] args)
        {
            var column = new ColumnOperation {ColumnId = "Name"};

            var x = (column + 2 == 4) & (column * 3 == 8);

            Console.WriteLine(x);


        }
    }
}
