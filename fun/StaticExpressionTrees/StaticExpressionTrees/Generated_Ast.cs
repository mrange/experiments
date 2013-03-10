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


    partial class ColumnOperation<T>
    {
        public static ComputeOperation<T> operator +(ColumnOperation<T> left, ColumnOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Add  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator +(ColumnOperation<T> left, ValueOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Add  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator +(ColumnOperation<T> left, ComputeOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Add  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator -(ColumnOperation<T> left, ColumnOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Subtract  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator -(ColumnOperation<T> left, ValueOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Subtract  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator -(ColumnOperation<T> left, ComputeOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Subtract  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator *(ColumnOperation<T> left, ColumnOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Multiply  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator *(ColumnOperation<T> left, ValueOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Multiply  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator *(ColumnOperation<T> left, ComputeOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Multiply  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator /(ColumnOperation<T> left, ColumnOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Divide  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator /(ColumnOperation<T> left, ValueOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Divide  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator /(ColumnOperation<T> left, ComputeOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Divide  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ColumnOperation<T> left, ColumnOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ColumnOperation<T> left, ValueOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ColumnOperation<T> left, ComputeOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ColumnOperation<T> left, ColumnOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ColumnOperation<T> left, ValueOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ColumnOperation<T> left, ComputeOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ColumnOperation<T> left, ColumnOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ColumnOperation<T> left, ValueOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ColumnOperation<T> left, ComputeOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ColumnOperation<T> left, ColumnOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ColumnOperation<T> left, ValueOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ColumnOperation<T> left, ComputeOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ColumnOperation<T> left, ColumnOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ColumnOperation<T> left, ValueOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ColumnOperation<T> left, ComputeOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ColumnOperation<T> left, ColumnOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ColumnOperation<T> left, ValueOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ColumnOperation<T> left, ComputeOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

    }
    partial class ValueOperation<T>
    {
        public static ComputeOperation<T> operator +(ValueOperation<T> left, ColumnOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Add  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator +(ValueOperation<T> left, ValueOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Add  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator +(ValueOperation<T> left, ComputeOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Add  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator -(ValueOperation<T> left, ColumnOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Subtract  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator -(ValueOperation<T> left, ValueOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Subtract  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator -(ValueOperation<T> left, ComputeOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Subtract  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator *(ValueOperation<T> left, ColumnOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Multiply  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator *(ValueOperation<T> left, ValueOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Multiply  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator *(ValueOperation<T> left, ComputeOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Multiply  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator /(ValueOperation<T> left, ColumnOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Divide  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator /(ValueOperation<T> left, ValueOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Divide  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator /(ValueOperation<T> left, ComputeOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Divide  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ValueOperation<T> left, ColumnOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ValueOperation<T> left, ValueOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ValueOperation<T> left, ComputeOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ValueOperation<T> left, ColumnOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ValueOperation<T> left, ValueOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ValueOperation<T> left, ComputeOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ValueOperation<T> left, ColumnOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ValueOperation<T> left, ValueOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ValueOperation<T> left, ComputeOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ValueOperation<T> left, ColumnOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ValueOperation<T> left, ValueOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ValueOperation<T> left, ComputeOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ValueOperation<T> left, ColumnOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ValueOperation<T> left, ValueOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ValueOperation<T> left, ComputeOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ValueOperation<T> left, ColumnOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ValueOperation<T> left, ValueOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ValueOperation<T> left, ComputeOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

    }
    partial class ComputeOperation<T>
    {
        public static ComputeOperation<T> operator +(ComputeOperation<T> left, ColumnOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Add  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator +(ComputeOperation<T> left, ValueOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Add  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator +(ComputeOperation<T> left, ComputeOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Add  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator -(ComputeOperation<T> left, ColumnOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Subtract  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator -(ComputeOperation<T> left, ValueOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Subtract  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator -(ComputeOperation<T> left, ComputeOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Subtract  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator *(ComputeOperation<T> left, ColumnOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Multiply  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator *(ComputeOperation<T> left, ValueOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Multiply  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator *(ComputeOperation<T> left, ComputeOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Multiply  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator /(ComputeOperation<T> left, ColumnOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Divide  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator /(ComputeOperation<T> left, ValueOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Divide  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static ComputeOperation<T> operator /(ComputeOperation<T> left, ComputeOperation<T> right)
        {
            return new ComputeOperation<T>
            {
                Type        = AstType.Divide  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ComputeOperation<T> left, ColumnOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ComputeOperation<T> left, ValueOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator ==(ComputeOperation<T> left, ComputeOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.EqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ComputeOperation<T> left, ColumnOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ComputeOperation<T> left, ValueOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator !=(ComputeOperation<T> left, ComputeOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.NotEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ComputeOperation<T> left, ColumnOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ComputeOperation<T> left, ValueOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <(ComputeOperation<T> left, ComputeOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ComputeOperation<T> left, ColumnOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ComputeOperation<T> left, ValueOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >(ComputeOperation<T> left, ComputeOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThan  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ComputeOperation<T> left, ColumnOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ComputeOperation<T> left, ValueOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator <=(ComputeOperation<T> left, ComputeOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.LessThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ComputeOperation<T> left, ColumnOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ComputeOperation<T> left, ValueOperation<T> right)
        {
            return new CompareOperation
            {
                Type        = AstType.GreaterThanOrEqualTo  ,
                Left        = left                  ,
                Right       = right                 ,
            };
        }

        public static CompareOperation operator >=(ComputeOperation<T> left, ComputeOperation<T> right)
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

