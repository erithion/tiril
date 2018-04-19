#pragma once

#include <utf8.h>
#include <memory>
#include <algorithm>
#include <boost/iterator.hpp>

namespace tiril::iterators::traversers
{
    enum { idBuffer = 0, idCurrentIdx, idCache };

    template < typename Buffer >
    struct as_string
    {
        typedef std::pair < size_t, size_t > bounds;
        typedef Buffer                       buffer_type;
        // Requirement of iterator_facade
        // string const in order to make the iterator read-only
        typedef std::string                  value_type;
        typedef std::string const            reference;

        // TODO: try to use if constexpr to eliminate buffer checks for non-end iterators
        value_type dereference( ) const
        {
            auto tuple = this->data( );
            auto buf = std::get< idBuffer >( tuple );
            if ( buf != 0 )
            {
                auto pair = std::get< idCache >( tuple )[std::get< idCurrentIdx >( tuple )];
                return value_type( buf->at( pair.first ), pair.second );
            }
            return value_type( );
        }

    protected:

        // The burden of keeping the data is to be laid upon the user of the strategy, i.s. the derived class. 
        // Lest we would have required to duplicate the data and its handling within each new strategy
        virtual std::tuple< buffer_type*, int, std::vector< bounds > > data( ) const = 0;
    };

    template < typename Buffer >
    struct as_bounds
    {
        typedef std::pair < size_t, size_t > bounds;
        typedef Buffer                       buffer_type;
        // Requirement of iterator_facade
        // string const in order to make the iterator read-only
        typedef bounds                       value_type;
        typedef bounds const                 reference;

        value_type dereference( ) const
        {
            auto tuple = this->data( );
            auto cache = std::get< idCache >( tuple );
            return cache.size( ) != 0 ? cache[std::get< idCurrentIdx >( tuple )] : value_type( );
        }

    protected:

        virtual std::tuple< buffer_type*, int, std::vector< bounds > > data( ) const = 0;
    };

    template < typename Buffer
        , template < typename > typename ReturnAs >
        struct generic
        : public ReturnAs< Buffer >
    {
        using typename ReturnAs< Buffer >::bounds;
        using typename ReturnAs< Buffer >::buffer_type;
        using predicate = std::function< bool( int ) >;

        // Vi er ikke redd å bevare "default constructor" for vi har allerede forbudt den i "iterator-superclass"
        generic( )
            : ReturnAs< Buffer >( )
            , buf_( )
            , cur_( )
            , pred_( )
            , cache_( ) {}

        generic( buffer_type buf, predicate predicateNotDelimiter )
            : ReturnAs< Buffer >( )
            , buf_( std::make_unique< buffer_type >( buf ) )
            , cur_( 0 )
            , pred_( predicateNotDelimiter )
            , cache_( cache( buf.at( 0 ), buf.at( 0 ) + buf.size( ), predicateNotDelimiter ) ) {}

        // Дозволимо копіювати навіть несумісні обєкти, покладемось на компілер,
        // адже ми все одно матимемо додатковий контроль над цим конструктором у суперкласі
        template < typename OtherBuffer
                 , template < typename > typename OtherReturn  >
            generic( const generic< OtherBuffer, OtherReturn >& other )
            : ReturnAs< Buffer >( )
            , buf_( other.buf_ )
            , cur_( other.cur_ )
            , pred_( other.pred_ )
            , cache_( other.cache_ ) {}

        void increment( )
        {
            ++this->cur_;
            // Hvis vi har nådd enden, må vi begynne på nytt
            if ( static_cast< size_t >( this->cur_ ) >= this->cache_.size( ) )
                this->cur_ = 0;
        }

        void decrement( )
        {
            --this->cur_;
            if ( this->cur_ < 0 )
                this->cur_ = this->cache_.size( ) - 1;
        }

        template < typename OtherValue, template < typename > typename Return >
        bool equal( generic< OtherValue, Return > const& other ) const
        {
            return this->buf_ == other.buf_ && this->cur_ == other.cur_;
        }

    private:
        template < typename OtherBuffer
                 , template < typename > typename OtherReturnAs >
            friend struct generic;

        std::shared_ptr< buffer_type >  buf_;
        int                             cur_;
        predicate                       pred_;
        // Since it is impossible to traverse thru UTF8 backwards correctly, 
        // we cannot therefore use reverse iterators to implement an operator --.
        // The most reasonable and cheap way to achieve this is to implement a cache,
        // which is being built upon the iterator construction.
        std::vector< bounds >           cache_;

        template < typename It >
        static std::vector< bounds > cache( It itBegin, It itEnd, const predicate& pred )
        {
            std::vector< bounds > ret( 0 );
            utf8::iterator< It >  begin( itBegin, itBegin, itEnd );
            utf8::iterator< It >  end( itEnd, itBegin, itEnd );
            while ( begin != end )
            {
                auto first = std::find_if( begin, end, pred );
                // Hvis vi har funnet ingenting før vi nådde enden, må vi begynne på nytt
                if ( first == end )
                    break;
                auto last = std::find_if_not( first, end, pred );
                ret.push_back( std::make_pair( first.base( ) - itBegin, last.base( ) - first.base( ) ) );
                begin = last;
            }
            return ret;
        }

        // Method the base classes can call to get themselves the data they need
        virtual std::tuple< buffer_type*, int, std::vector< bounds > > data( ) const override
        {
            return std::make_tuple( this->buf_.get( ), this->cur_, this->cache_ );
        }
    };
}
