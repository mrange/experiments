// test.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include <functional>

template<typename T>
using func = std::function<T ()>;

template<typename T>
func<T> make1 (T && v)
{
    return [i = std::forward<T> (v)] () mutable
    {
        return std::move (i);
    };
}

template<typename T>
func<func<T>> make2 (func<T> f)
{
    return [i = std::move (f)] () mutable
    {
        T v = i ();
        return [ii = std::move (v)] () mutable
        {
            return std::move (ii);
        };
    };
}

int main()
{
    auto f1 = make1 (1);
    auto v1 = f1 ();

    auto f2 = make2 (f1);
    auto v2 = f2 () ();

	return 0;
}

