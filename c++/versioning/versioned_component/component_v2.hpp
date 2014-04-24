#pragma once

#include "common_v1.hpp"
#include "versioned_component.hpp"

namespace my_stuff
{
    namespace component
    {
        namespace v2
        {
            struct VERSIONED_COMPONENT_API customer
            {
                customer (
                        my_stuff::common::v1::string_type first_name
                    ,   my_stuff::common::v1::string_type last_name
                    )
                    : first_name(std::move (first_name))
                    , last_name (std::move (last_name))
                {
                }

                int get_version () const { return 2; }

                my_stuff::common::v1::string_type first_name;
                my_stuff::common::v1::string_type last_name ;
            };
        }
    }
}