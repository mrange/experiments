// ----------------------------------------------------------------------------
#include "stdafx.h"
// ----------------------------------------------------------------------------
#include <cassert>
#include <functional>
#include <memory>
#include <string>
#include <typeinfo>
#include <typeindex>
#include <type_traits>
#include <unordered_map>
// ----------------------------------------------------------------------------
#define DI_ASSERT assert
#define DI_NON_COPYABLE_MOVEABLE(type)                  \
            type (type const &)             = delete;   \
            type& operator= (type const &)  = delete;   \
            type (type &&)                  = delete;   \
            type& operator= (type &&)       = delete;
#define DI_NON_COPYABLE(type)                  \
            type (type const &)             = delete;   \
            type& operator= (type const &)  = delete;   
// ----------------------------------------------------------------------------
namespace di
{
    struct dependency_resolver 
    {
        using ptr = std::shared_ptr<dependency_resolver>;

        virtual ~dependency_resolver () {}

        DI_NON_COPYABLE_MOVEABLE (dependency_resolver);

        template<typename TDependency> 
        inline TDependency* resolve_dependency ()
        {
            using TCleaned = std::remove_cv<TDependency>::type;
            auto resolved = on_resolve_dependency (typeid (TCleaned));
            return static_cast<TDependency*> (resolved);
        }

    protected:
        dependency_resolver () = default;
        virtual void * on_resolve_dependency (std::type_index const & ti) = 0;
    };

    struct auto_dependency_resolver 
    {
        explicit auto_dependency_resolver (dependency_resolver::ptr const & resolver)
            : resolver (resolver)
        {
            DI_ASSERT (resolver);
        }

        DI_NON_COPYABLE (auto_dependency_resolver);

        auto_dependency_resolver (auto_dependency_resolver && o)
            :   resolver (std::move (o.resolver))
        {
        }

        auto_dependency_resolver& operator= (auto_dependency_resolver && o)
        {
            resolver = std::move (o.resolver);
        }

        template<typename TDependencyPtr>
        operator TDependencyPtr ()
        {
            static_assert (std::is_pointer<TDependencyPtr>::value, "Can only auto resolve to pointer types");
            using TDependency = std::remove_pointer<TDependencyPtr>::type;
            DI_ASSERT (resolver);
            return resolver->resolve_dependency<TDependency> ();
        }

    private:
        dependency_resolver::ptr resolver;
    };


    struct dependency_register
    {
        using ptr = std::shared_ptr<dependency_register>;

        virtual ~dependency_register () {}

        DI_NON_COPYABLE_MOVEABLE (dependency_register);

        virtual dependency_resolver::ptr get_resolver ()            = 0;
        virtual dependency_register::ptr create_child_register ()   = 0;

        template<typename TDependency>
        inline bool register_dependency (TDependency * dependency)
        {
            return on_register_dependency (typeid (TDependency), dependency);
        }

    protected:
        dependency_register () = default;
        virtual bool on_register_dependency (std::type_index const & ti, void * dependency) = 0;
    };

    namespace details 
    {
        struct dependency_register_impl : dependency_register
                                        , dependency_resolver
                                        , std::enable_shared_from_this<dependency_register_impl>
        {
            using impl_ptr          = std::shared_ptr<dependency_register_impl>;
            using dependency_map    = std::unordered_map<std::type_index, void *>;
            dependency_register_impl (impl_ptr const & parent = impl_ptr ())
                :   parent (parent)
            {
            }

            DI_NON_COPYABLE_MOVEABLE (dependency_register_impl);

            dependency_resolver::ptr get_resolver () override
            {
                return shared_from_this ();
            }

            dependency_register::ptr create_child_register () override
            {
                return std::make_shared<dependency_register_impl> (shared_from_this ());
            }

        protected:
            bool on_register_dependency (std::type_index const & ti, void * dependency) override
            {
                auto result = dependencies.emplace (ti, dependency);
                return result.second;
            }

            void * on_resolve_dependency (std::type_index const & ti) override
            {
                auto find = dependencies.find (ti);
                if (find != dependencies.end ())
                {
                    return find->second;
                }
                else if (parent)
                {
                    return parent->on_resolve_dependency (ti);
                }
                else
                {
                    return nullptr;
                }
            }
        private:
            impl_ptr        parent          ;
            dependency_map  dependencies    ;
        };
    }

    dependency_register::ptr create_dependency_register ()
    {
        auto reg = std::make_shared<details::dependency_register_impl> ();
        reg->register_dependency<dependency_register> (reg.get ());
        return reg;
    }

}
// ----------------------------------------------------------------------------
#define LOG(alog,msg) alog->log (__FILE__, __LINE__, msg)
// ----------------------------------------------------------------------------
namespace
{
    struct logger
    {
        virtual ~logger () {}
        virtual void log (char const * file_name, unsigned int line_no, char const * message) = 0;
    };


    using event_handler = std::function<void ()>;
    struct event_distributor
    {
        virtual ~event_distributor () {}
        virtual void subscribe_to (std::string const & event_name, event_handler const & handler) = 0;
    };

    struct std_logger : logger
    {
        std_logger (di::dependency_resolver::ptr const &)
        {
        }

        void log (char const * file_name, unsigned int line_no, char const * message) override
        {
            printf ("%s:%u - %s\n", file_name, line_no, message);
        }
    };

    struct std_event_distributor : event_distributor
    {
        std_event_distributor (di::dependency_resolver::ptr const & resolver)
            :   std_event_distributor (resolver->resolve_dependency<logger> ())
        {
        }

        std_event_distributor (logger * log)
            : log (log)
        {
        }

        void subscribe_to (std::string const & event_name, event_handler const & handler) override
        {
            LOG (log, "subscribe_to");
        }
    private:
        logger* log;
    };

    void do_stuff (di::dependency_resolver::ptr & resolver)
    {
        auto auto_resolver              = di::auto_dependency_resolver (resolver);

        di::dependency_register *   reg = auto_resolver;
        logger                  *   log = auto_resolver;
        event_distributor       *   ed  = auto_resolver;

        auto child_reg                  = reg->create_child_register ();
        auto child_res                  = child_reg->get_resolver ();
        {
            auto auto_resolver              = di::auto_dependency_resolver (child_res);

            di::dependency_register *   reg = auto_resolver;
            logger                  *   log = auto_resolver;
            event_distributor       *   ed  = auto_resolver;
        }

        LOG (log, "Testing");
        ed->subscribe_to ("TEST", []() {});
    }

}
// ----------------------------------------------------------------------------
int main ()
{
    // ------------------------------------------------------------------------
    auto reg = di::create_dependency_register ();
    auto res = reg->get_resolver ();

    std_logger log (res);
    reg->register_dependency<logger> (&log);

    std_event_distributor ed (res);
    reg->register_dependency<event_distributor> (&ed);
    // ------------------------------------------------------------------------

    // ------------------------------------------------------------------------
    do_stuff (res);
    // ------------------------------------------------------------------------

    return 0;
}
// ----------------------------------------------------------------------------
