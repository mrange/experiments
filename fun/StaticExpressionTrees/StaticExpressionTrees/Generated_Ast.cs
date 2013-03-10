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

// ############################################################################
// #                                                                          #
// #        ---==>  T H I S  F I L E  I S   G E N E R A T E D  <==---         #
// #                                                                          #
// # This means that any edits to the .cs file will be lost when its          #
// # regenerated. Changes should instead be applied to the corresponding      #
// # template file (.tt)                                                      #
// ############################################################################







namespace StaticExpressionTrees
{
    enum AstType
    {
        Value,
        Column,
        Add,
        Subtract,
        Multiply,
        Divide,
        EqualTo,
        NotEqualTo,
        LessThan,
        GreaterThan,
        LessThanOrEqualTo,
        GreaterThanOrEqualTo,
        And,
        Or,
        
    }


    partial class ColumnOperation
    {
        public static ComputeOperation operator +(ColumnOperation left, ColumnOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Add  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator +(ColumnOperation left, ValueOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Add  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator +(ColumnOperation left, ComputeOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Add  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator -(ColumnOperation left, ColumnOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Subtract  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator -(ColumnOperation left, ValueOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Subtract  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator -(ColumnOperation left, ComputeOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Subtract  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator *(ColumnOperation left, ColumnOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Multiply  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator *(ColumnOperation left, ValueOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Multiply  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator *(ColumnOperation left, ComputeOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Multiply  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator /(ColumnOperation left, ColumnOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Divide  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator /(ColumnOperation left, ValueOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Divide  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator /(ColumnOperation left, ComputeOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Divide  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ColumnOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ColumnOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ColumnOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ColumnOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ColumnOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ColumnOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ColumnOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ColumnOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ColumnOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ColumnOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ColumnOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ColumnOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ColumnOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ColumnOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ColumnOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ColumnOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ColumnOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ColumnOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ColumnOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ColumnOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ColumnOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ColumnOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ColumnOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ColumnOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

    }
    partial class ValueOperation
    {
        public static ComputeOperation operator +(ValueOperation left, ColumnOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Add  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator +(ValueOperation left, ValueOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Add  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator +(ValueOperation left, ComputeOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Add  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator -(ValueOperation left, ColumnOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Subtract  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator -(ValueOperation left, ValueOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Subtract  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator -(ValueOperation left, ComputeOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Subtract  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator *(ValueOperation left, ColumnOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Multiply  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator *(ValueOperation left, ValueOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Multiply  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator *(ValueOperation left, ComputeOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Multiply  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator /(ValueOperation left, ColumnOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Divide  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator /(ValueOperation left, ValueOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Divide  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator /(ValueOperation left, ComputeOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Divide  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ValueOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ValueOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ValueOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ValueOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ValueOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ValueOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ValueOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ValueOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ValueOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ValueOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ValueOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ValueOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ValueOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ValueOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ValueOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ValueOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ValueOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ValueOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ValueOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ValueOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ValueOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ValueOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ValueOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ValueOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

    }
    partial class ComputeOperation
    {
        public static ComputeOperation operator +(ComputeOperation left, ColumnOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Add  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator +(ComputeOperation left, ValueOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Add  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator +(ComputeOperation left, ComputeOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Add  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator -(ComputeOperation left, ColumnOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Subtract  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator -(ComputeOperation left, ValueOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Subtract  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator -(ComputeOperation left, ComputeOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Subtract  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator *(ComputeOperation left, ColumnOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Multiply  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator *(ComputeOperation left, ValueOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Multiply  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator *(ComputeOperation left, ComputeOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Multiply  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator /(ComputeOperation left, ColumnOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Divide  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator /(ComputeOperation left, ValueOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Divide  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation operator /(ComputeOperation left, ComputeOperation right)
        {
            return new ComputeOperation
            {
                Type        = AstType.Divide  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ComputeOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ComputeOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ComputeOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ComputeOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ComputeOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ComputeOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ComputeOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ComputeOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ComputeOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ComputeOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ComputeOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ComputeOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ComputeOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ComputeOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ComputeOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ComputeOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ComputeOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ComputeOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ComputeOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ComputeOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ComputeOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ComputeOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ComputeOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ComputeOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

    }
    partial class CompareOperation
    {
        public static CompareOperation operator ==(CompareOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(CompareOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(CompareOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(CompareOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(CompareOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(CompareOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(CompareOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(CompareOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(CompareOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(CompareOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(CompareOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(CompareOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(CompareOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(CompareOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(CompareOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(CompareOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(CompareOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(CompareOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(CompareOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(CompareOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(CompareOperation left, ColumnOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(CompareOperation left, ValueOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(CompareOperation left, ComputeOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(CompareOperation left, CompareOperation right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static LogicalOperation operator &(CompareOperation left, CompareOperation right)
        {
            return new LogicalOperation
            {
                Type        = AstType.And  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static LogicalOperation operator &(CompareOperation left, LogicalOperation right)
        {
            return new LogicalOperation
            {
                Type        = AstType.And  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static LogicalOperation operator |(CompareOperation left, CompareOperation right)
        {
            return new LogicalOperation
            {
                Type        = AstType.Or  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static LogicalOperation operator |(CompareOperation left, LogicalOperation right)
        {
            return new LogicalOperation
            {
                Type        = AstType.Or  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

    }
    partial class LogicalOperation
    {
        public static LogicalOperation operator &(LogicalOperation left, CompareOperation right)
        {
            return new LogicalOperation
            {
                Type        = AstType.And  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static LogicalOperation operator &(LogicalOperation left, LogicalOperation right)
        {
            return new LogicalOperation
            {
                Type        = AstType.And  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static LogicalOperation operator |(LogicalOperation left, CompareOperation right)
        {
            return new LogicalOperation
            {
                Type        = AstType.Or  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static LogicalOperation operator |(LogicalOperation left, LogicalOperation right)
        {
            return new LogicalOperation
            {
                Type        = AstType.Or  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

    }
}

