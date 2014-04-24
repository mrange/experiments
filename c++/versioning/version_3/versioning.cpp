#include "stdafx.h"

#include <stdio.h>

#pragma comment(lib, "versioned_component")

#include "api_v2.hpp"
#include "api_v3.hpp"

using namespace my_stuff::v3;

int main ()
{
    auto c = customer (L"First", L"Last");
        
    printf ("Version: %d\r\n", c.get_version ());

    return 0;
}

