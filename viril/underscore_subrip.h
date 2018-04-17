#pragma once

#include <memory>
#include <boost/utility/string_view.hpp>
#include "iterators.h"
#include "buffer_subrip.h"
#include "vlc_helper.h"

namespace tiril::underscore
{
    void for_subrip( tiril::iterators::circular_bidirectional_bounds< tiril::buffers::subrip >::bounds word, subpicture_updater_sys_region_t& r )
    {
        auto start = r.p_segments;
        size_t idx = 0;
        while ( start != 0 )
        {
            boost::string_view view( start->psz_text );

            if ( start->style == 0 )
                start->style = text_style_Create( STYLE_HAS_FLAGS );

            start->style->i_style_flags &= ~STYLE_UNDERLINE;

            if ( word.first >= idx && word.first < idx + view.size( ) /* after the modifications below */ && word.second != 0 )
            {
                auto init = view.substr( 0, word.first - idx ).to_string( );
                auto sel = view.substr( word.first - idx, word.second ).to_string( );
                auto tail = view.substr( word.first - idx + word.second ).to_string( );

                text_segment_t* selected = start;
                if ( !init.empty( ) )
                {
                    selected = text_segment_New( sel.c_str( ) );
                    selected->style = text_style_Create( STYLE_HAS_FLAGS );
                    selected->p_next = start->p_next;
                    start->p_next = selected;
                    start->psz_text[init.size( )] = 0;
                }
                selected->style->i_style_flags |= STYLE_UNDERLINE;
                selected->psz_text[sel.size( )] = 0;

                if ( !tail.empty( ) )
                {
                    auto last = text_segment_New( tail.c_str( ) );
                    last->p_next = selected->p_next;
                    selected->p_next = last;
                }
                word.first += sel.size( );
                word.second -= sel.size( );
                idx += sel.size( );
                start = selected->p_next;
            }
            else
            {
                idx += view.size( );
                start = start->p_next;
            }
        }
    }
}