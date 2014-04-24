#include "stdafx.h"

#include <stdio.h>

#pragma comment(lib, "versioned_component")

#include "api_v1.hpp"

using namespace my_stuff::v1;

int main ()
{
    auto c = customer ("First&Last");
        
    printf ("Version: %d\r\n", c.get_version ());

    return 0;
}

