// test.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include <functional>

template<typename TA>
std::function<std::function<void (TA &&)> (int x)> make ()
{
    return [] (int x)
    {
        return [] (TA && v) 
        {
            printf ("Hi there");
        };
    };
}

int main()
{
    auto f1 = make<int> ();

	return 0;
}

