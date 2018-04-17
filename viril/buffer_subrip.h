#pragma once

#include <vector>
#include <functional>
#include <boost/tuple/tuple.hpp>

#include "vlc_helper.h"

namespace tiril::buffers
{
    // Any changes to the underlying subpicture's region do not invalidate the buffer unless
    // the inner text has been changed or the subpicture itself has been deleted
    struct subrip
    {
        using region_ref = std::reference_wrapper< subpicture_updater_sys_region_t >;

        subrip( ) = delete;

        explicit subrip( region_ref reg )
            : reg_( reg )
            , str_( as( boost::make_tuple( 0, length( reg.get( ).p_segments ), reg.get( ).p_segments ) ) ) { }

        const char* at( size_t idx ) const { return str_.data( ) + idx; }

        size_t size( ) const { return str_.size(); }

        // We don't compare strings since such a comparison looks excessive for the time being
        bool operator ==( const subrip& other ) const { return reg_.get().p_segments == other.reg_.get().p_segments; }

    private:
        enum { segIdx = 0, segLen, segPtr };
        using segment = boost::tuple< size_t, size_t, text_segment_t* >;

        region_ref  reg_;
        // We'll use this for search algorithms since indicies of the content still remain unchanged
        std::string str_;

        static size_t length( const text_segment_t* s ) 
        { 
            return s && s->psz_text ? strlen( s->psz_text ) : 0; 
        }

        static segment next( const segment& cur )
        {
            auto s = cur.get< segPtr >( ) ? cur.get< segPtr >( )->p_next : 0;
            return boost::make_tuple( cur.get< segIdx >( ) + cur.get< segLen >( ), length( s ), s );
        }

        static std::string as( const segment& cur )
        {
            std::string ret;
            for ( auto i = cur; i.get< segLen >( ) != 0; i = next( i ) )
                ret += i.get< segPtr >( )->psz_text;
            return ret;
        }

        // Denne funksjonen når en listens ende. Hvis det har skjedd, får man bare idx som ikke er null
        static segment at( region_ref reg, size_t idx )
        {
            auto cur = boost::make_tuple( 0, length( reg.get( ).p_segments ), reg.get( ).p_segments );
            while ( idx >= cur.get< segIdx >( ) + cur.get< segLen >( ) &&
                    cur.get< segLen >( ) != 0 )
                cur = next( cur );
            return cur;
        }
    };
}