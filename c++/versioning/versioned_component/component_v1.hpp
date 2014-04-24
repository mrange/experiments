#pragma once

#include "common_v1.hpp"
#include "versioned_component.hpp"

namespace my_stuff
{
    namespace component
    {
        namespace v1
        {
            struct VERSIONED_COMPONENT_API customer
            {
                customer (
                        my_stuff::common::v1::string_type full_name
                    )
                    : full_name (std::move (full_name))
                {
                }

                int get_version () const { return 1; }

                my_stuff::common::v1::string_type full_name;
            };

        }
    }
}