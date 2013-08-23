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
using StaticExpressionTrees.FLinq;

namespace StaticExpressionTrees
{

    class CustomerTableDescriptor
    {
        public readonly ColumnOperation<long>    Id         = new ColumnOperation<long>     {ColumnId = "Id"        };
        public readonly ColumnOperation<string>  FirstName  = new ColumnOperation<string>   {ColumnId = "FirstName" };
        public readonly ColumnOperation<string>  LastName   = new ColumnOperation<string>   {ColumnId = "LastName"  };
    }

    static class StaticExpressionExtensions
    {
        public static 
    }

    class Program
    {
        static void Main(string[] args)
        {
            var column = new ColumnOperation<int> {ColumnId = "Name"};

            ComputeOperation<int> y = column + 2;

            LogicalOperation x = (column + 2 == 4) & (column * 3 == 8);

            Console.WriteLine(x);


        }
    }
}
