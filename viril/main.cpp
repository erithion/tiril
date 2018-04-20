#include <cctype>
#include <memory>
#include <mutex>
#include <locale>
#include <functional>
#include <boost/ref.hpp>
#include <boost/range/as_literal.hpp>
#include <boost/algorithm/string/find.hpp>

#include "vlc_helper.h"
#include "tiril.h"
#include "buffer_subrip.h"
#include "iterators.h"
#include "underscore_subrip.h"

struct ui : public vlc_object_t { };

vlc_module_begin( )
    set_shortname( N_( "Viril" ) )
    set_subcategory( SUBCAT_VIDEO_SUBPIC )
    set_category( CAT_INPUT )
        set_capability( "spu decoder", 500 )
        set_callbacks( tiril::module< decoder_t >::open, tiril::module< decoder_t >::close )
        set_description( N_( "Viril - decoder hook" ) )
    add_submodule( )
        set_callbacks( tiril::module< filter_t >::open, tiril::module< filter_t >::close )
        set_capability( "sub filter", 0 )
        set_description( N_( "Viril, a daughter of Tiril, a language learning assistant software" ) )
    add_submodule( )
        set_callbacks( tiril::module< ui >::open, tiril::module< ui >::close )
        set_capability( "interface", 300 )
        set_description( N_( "Viril - UI hook" ) )

vlc_module_end( )

/* TODO
    - mouse hover must underline a word, mouse click shows a translation,
            shift+click adds (underlines) a word to other underlined previously,
            shift+left mouse down edits current word selection by letters until mouse up is pressed
    - find how to hold subpic in vlc until i have finished with it
      Res: Ok, for subsdec there is no way to hold a subpicture in destroy fn. But there is in ass.
           One possible solution is to hook subsdec destroy function and use mutex

   Useful:
    - C++17 new features: https://stackoverflow.com/questions/38060436/what-are-the-new-features-in-c17
    - C++17 by github examples: https://github.com/AnthonyCalandra/modern-cpp-features
*/

using subrip_buf = tiril::buffers::subrip;
using subrip_bounds_iterator = tiril::iterators::circular_bidirectional_bounds< subrip_buf >;
using subrip_string_iterator = tiril::iterators::circular_bidirectional_string< subrip_buf >;

// Vi trenger ikke "virtual interfaces" mer. 
// "Template"-variabler tillater oss å bruke flere forskjellige variabler som en
template < typename It >
static It               iter = tiril::iterators::end< subrip_bounds_iterator >( );
static std::mutex		mutex;

void POC______StartSelection( subpicture_t*	pic )
{
    subpicture_updater_sys_t2* upd = reinterpret_cast< subpicture_updater_sys_t2* >( pic->updater.p_sys );

    vlc_mutex_lock( &upd->p_dec_sys->lock );

    ASS_Track* track = upd->p_dec_sys->p_track;
    ASS_Event* event = track->events + track->n_events - 1;

    if ( !boost::find_first( event->Text, "{\\u1" ) )
    {
        std::string s = std::string( "{\\u1}" ) + std::string( event->Text ) + std::string( "{\\u0}" );
        libvlc_free( event->Text );
        event->Text = mY_hACkY_wACky_StRDuP( s.data( ) );
    }
    vlc_mutex_unlock( &upd->p_dec_sys->lock );
}
void POC______ChangeSelection( subpicture_t*	pic )
{
    subpicture_updater_sys_t2* tmp = reinterpret_cast< subpicture_updater_sys_t2* >( pic->updater.p_sys );
    vlc_mutex_lock( &tmp->p_dec_sys->lock );

    ASS_Track* tr = tmp->p_dec_sys->p_track;
    ASS_Event* e1 = tr->events + tr->n_events - 1;

    boost::iterator_range< char* > result = boost::find_first( e1->Text, "{\\u" );

    if ( result )
    {
        auto it = result.end( );
        if ( *it == '1' ) *it = '0';
        else if ( *it == '0' ) *it = '1';
    }

    tiril::subpic::redraw( pic );
    vlc_mutex_unlock( &tmp->p_dec_sys->lock );
}

subpicture_t* filter( filter_t* flt, subpicture_t* pic )
{
    if ( pic->b_subtitle )
    {
        std::scoped_lock< std::mutex > guard( mutex );
        auto                           codec = tiril::spu_codec::current( flt );

        tiril::subpic::current( pic );

        // We do not support anything else for now. 
        // For example for avcodec with VLC_CODEC_BD_PG it would have crashed without this "if", 
        // since there's no textual information to read or update
        if ( codec == VLC_CODEC_SUBT )
        {
            // Upon receiving a new subtitle we are resetting the iterator to an empty one
            iter< subrip_bounds_iterator > = tiril::iterators::end< subrip_bounds_iterator >( );
            tiril::subpic::redraw_srt( pic );
        }
        else if ( codec == VLC_CODEC_SSA )
        {
            POC______StartSelection( pic );
            tiril::subpic::redraw( pic );
        }
    }
    return pic;
}

int onkey( vlc_object_t* flt, char const* action, vlc_value_t, vlc_value_t newval, void* )
{
    // Níl P0091R3 ag obair anois fós. Deir MSDN é sin :(
    // std::scoped_lock guard( mutex );
    std::scoped_lock< std::mutex > guard( mutex );
    auto                           subpic = tiril::subpic::current( );

    if ( subpic == 0 )
        return VLC_EGENERIC;

    if ( newval.i_int == KEY_INSERT )
    {
        // In case it was active
        // Debug and see why deactivate-activate doesn't work as expected
//        tiril::extension::deactivate( tiril::extension::manager( flt ), "viril.lua" );
        auto word = *subrip_string_iterator( iter< subrip_bounds_iterator > );
        tiril::extension::var_string( flt, "tiril_word", word );
        tiril::extension::activate( tiril::extension::manager( flt ), "viril.lua" );
    }
    else if ( newval.i_int == KEY_DELETE )
        tiril::extension::deactivate( tiril::extension::manager( flt ), "viril.lua" );

    switch ( tiril::spu_codec::current( flt ) )
    {
        case VLC_CODEC_SUBT:
        {
            if ( ( newval.i_int == KEY_END || newval.i_int == KEY_HOME ) 
               && iter< subrip_bounds_iterator > == tiril::iterators::end< subrip_bounds_iterator >( ) )
                // Setting up the real iterator if the user has pressed a key for the first time
                iter< subrip_bounds_iterator > = subrip_bounds_iterator( subrip_buf( std::ref( subpic->updater.p_sys->region ) ), [] ( int a ) -> bool
            {
                return std::isalpha( a , std::locale("") );
            } );
            // Otherwise we just start moving the pointer
            else if ( newval.i_int == KEY_END ) ++iter< subrip_bounds_iterator >;
            else if ( newval.i_int == KEY_HOME ) --iter< subrip_bounds_iterator >;

            tiril::underscore::for_subrip( *iter< subrip_bounds_iterator >, subpic->updater.p_sys->region );
            tiril::subpic::redraw_srt( subpic );
        }
        break;
        case VLC_CODEC_SSA:
        {
            if ( newval.i_int == KEY_END )
                POC______ChangeSelection( subpic );
        }
    }

    return VLC_SUCCESS;
}

template <>
int tiril::module< filter_t >::open( module< filter_t >::base_type* flt )
{
    flt->pf_sub_filter = filter;
    var_AddCallback( flt->obj.libvlc, "key-pressed", onkey, flt );

    return VLC_SUCCESS;
}

template <>
void tiril::module< filter_t >::close( module< filter_t >::base_type* filter )
{
    var_DelCallback( filter->obj.libvlc, "key-pressed", onkey, filter );
}

template <>
int tiril::module< decoder_t >::open( module< decoder_t >::base_type* decoder )
{
    // Власне те, заради чого ми вдаємо з себе "справжнісінький" декодер.
    // Нам вкрай необхідна інфа щодо поточного декодеру титрів,
    // бо кожен з них має власні структури, себто легко можна ускочити у халепу,
    // намагаючись розпарсити структуру libass-модуля кодом для парсингу subsdec-титрів.
    // Запит надходить сюди щоразу як змінюється кодек титрів.
    tiril::spu_codec::current( decoder, decoder->fmt_in.i_codec );

    // Vi gir den GENERIC koden tilbake for vi ikke har lyst til å ha blitt en ekte dekoder.
    // Slik har vi bare latt som at vi er en for å få nødvendig informasjon.
    return VLC_EGENERIC;
}

template <>
void tiril::module< decoder_t >::close( module< decoder_t >::base_type* )
{
    // We may have been called in response to the codec change, so we better reset the current subpicture at once
    std::scoped_lock< std::mutex > guard( mutex );
    tiril::subpic::current( 0 );
}

// Cheating to get interface object
template <>
int tiril::module< ui >::open( module< ui >::base_type* intf )
{
    tiril::extension::intf( intf );
    return VLC_EGENERIC;
}

template <>
void tiril::module< ui >::close( module< ui >::base_type* ) { }
