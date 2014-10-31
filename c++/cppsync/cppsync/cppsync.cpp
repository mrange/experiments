#include "stdafx.h"

#include <functional>
#include <stdio.h>
#include <string>

struct empty
{

};

enum class cancel_reason
{
    unknown  = 0x0001   ,
};

struct async_context
{

};

template<typename TValue>
using on_value      = std::function<void (TValue &&)>           ;
using on_error      = std::function<void (std::exception const &)>;
using on_cancelled  = std::function<void (cancel_reason)>       ;

template<typename TValue>
using async             = std::function<void (
    async_context       &       ,
    on_value<TValue>    const & ,
    on_error            const & ,
    on_cancelled        const & )>;


template<typename TValue>
async<TValue> async_return (TValue && value)
{
    return
        [iv = std::forward<TValue> (value)]
        (auto ctx, auto ov, auto oe, auto oc) mutable
        {
            ov (std::move (iv));
        };
}

template<typename TA, typename TB>
using binder    = std::function<async<TB> (TA &&)>;


template<typename TA, typename TB>
async<TB> async_bind (async<TA> a, binder<TA, TB> binder)
{
    return
        [ia = std::move (a), ibinder = std::move (binder)]
        (auto ctx, auto ov, auto oe, auto oc) mutable
        {
            auto fov = [&ctx, &ov, &oe, &oc, iibinder = std::move (ibinder)] (TA && v)
            {
                auto b = iibinder (std::move (v));
                b (ctx, ov, oe, oc);
            };

            ia (ctx, fov, oe, oc);
        };
}

int main ()
{
    auto a = async_return (std::string ("Test"));
    auto b = async_bind<std::string, std::string> (a, [] (std::string && a) { return async_return ("Testing_" + a); });

    async_context ctx;

    auto on_string = [] (std::string && v)
    {
        printf ("Value: %s\n", v.c_str ());
    };

    auto on_error = [] (std::exception const & e)
    {
        printf ("Exception: %s\n", e.what ());
    };

    auto on_cancelled = [] (cancel_reason cr)
    {
        printf ("Cancelled: %d\n", cr);
    };

    a (ctx, on_string, on_error, on_cancelled);

    return 0;
}