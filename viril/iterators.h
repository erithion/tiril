#pragma once

#include <boost/iterator.hpp>

#include "iterator_traversers.h"

namespace tiril::iterators
{
    template < typename Buffer
             , template < typename, template <typename> typename > typename Traverser
             , template < typename > typename ReturnAs  >
    struct circular_bidirectional
        : public boost::iterator_facade
                < circular_bidirectional< Buffer, Traverser, ReturnAs >
                , typename Traverser< Buffer, ReturnAs >::value_type
                , boost::bidirectional_traversal_tag
                , typename Traverser< Buffer, ReturnAs >::reference >
        , public Traverser< Buffer, ReturnAs >
    {
        using typename Traverser< Buffer, ReturnAs >::buffer_type;
        // Making them visible for iterator_facade 
        using typename Traverser< Buffer, ReturnAs >::value_type;
        using typename Traverser< Buffer, ReturnAs >::reference;

        // TODO: Переконатися що rvalues (&&) у конструкторі копіювання переміщуються замість копіювання

        template < typename ... Args >
        explicit circular_bidirectional( Args ... args )
            : Traverser< buffer_type, ReturnAs >( args... ) {}

        // TODO: enable_if-is_same на OtherBuffer
        template < typename OtherBuffer
                 , template < typename, template < typename > typename > typename OtherTraverser
                 , template < typename > typename OtherReturnAs  >
        circular_bidirectional( const circular_bidirectional< OtherBuffer, OtherTraverser, OtherReturnAs >& other )
            : Traverser< buffer_type, ReturnAs >( other ) {}

        // To produce an empty iterator
        struct enable_end {};

        // Vi lar kun "constructor"en med enable_end for å skape eksemplaret til den tomme "iterator"en
        template < typename T >
        circular_bidirectional( T )
            : Traverser< buffer_type, ReturnAs >()
        {
            if constexpr ( !std::is_same_v< T, enable_end > )
                static_assert( is_none );
        }
    private:
        friend class boost::iterator_core_access;
    };

    // Helpers
    template < typename Buffer >
    using circular_bidirectional_string = circular_bidirectional < Buffer, traversers::generic, traversers::as_string >;

    template < typename Buffer >
    using circular_bidirectional_bounds = circular_bidirectional < Buffer, traversers::generic, traversers::as_bounds >;

    template < typename Iter >
    Iter end( ) { return Iter( Iter::enable_end() ); }
}
