#include <boost/filesystem/path.hpp>
#include <boost/utility/string_view.hpp>
#include "vlc_helper.h"
#include "tiril.h"

static const char spuVariableName[] = "tiril_spu_codec";
static const char uiManagerVariableName[] = "tiril_intf";
std::atomic< subpicture_t* > tiril::subpic::subpic_ = 0;

int64_t tiril::spu_codec::current( vlc_object_t* any )
{
    return var_GetInteger( any->obj.libvlc, spuVariableName );
}

void tiril::spu_codec::current( vlc_object_t* any, spu_codec::fourcc codec )
{
    if ( int64_t fmt = tiril::spu_codec::current( any ); fmt != codec )
    {
        if ( fmt == 0 )
            var_Create( any->obj.libvlc, spuVariableName, VLC_VAR_INTEGER );

        var_SetInteger( any->obj.libvlc, spuVariableName, codec );
    }
}

subpicture_t* tiril::subpic::current( )
{
    return subpic_.load();
}

void tiril::subpic::current( subpicture_t* pic )
{
    subpic_.store( pic );
}

void tiril::subpic::redraw( subpicture_t* pic )
{
    if ( pic != 0 )
    {
        // Is féidir linn "update" mar sin
        struct subpicture_private_t
        {
            video_format_t src;
            video_format_t dst;
        };

        video_format_t frm = { 0 };
        auto priv = reinterpret_cast< subpicture_private_t* >( pic->p_private );
        video_format_Copy( &frm, &priv->dst );
        frm.i_chroma += 1;
        subpicture_Update( pic, &frm, &priv->dst, pic->i_start );
    }
}

void tiril::subpic::redrawSrt( subpicture_t* pic )
{
    if ( pic != 0 )
    {
        subpicture_updater_sys_t* sys = pic->updater.p_sys;
        sys->i_next_update = pic->i_start + 1;
        sys->b_blink_even = !sys->b_blink_even;
    }
}

extensions_manager_t* tiril::extension::manager( vlc_object_t* any )
{
    auto                            intf = reinterpret_cast< vlc_object_t* >( var_GetInteger( any->obj.libvlc, uiManagerVariableName ) );
    vlc_list_t*                     list = vlc_list_children( reinterpret_cast< vlc_object_t* >( &intf->obj ) );
    tiril::scoped_do< vlc_list_t >  release_( list, [] ( vlc_list_t* p )
    {
        if ( p != 0 )
            vlc_list_release( p );
    } );
    // It must be only one element. And it must be "lua". Otherwise we have found something different from lua extension manager
    if ( list->i_count == 1 )
    {
        auto mgr = reinterpret_cast< extensions_manager_t * >( list->p_values[0].p_address );
        auto internal = reinterpret_cast < vlc_object_internals_t* >( &mgr->obj - sizeof( extensions_manager_t* ) );
        boost::string_view view( internal->psz_name, 3 );
        if ( view == "lua" )
            return mgr;
    }
    return 0;
}

void tiril::extension::intf( vlc_object_t* obj )
{
    if ( var_GetInteger( obj->obj.libvlc, uiManagerVariableName ) == 0 )
        var_Create( obj->obj.libvlc, uiManagerVariableName, VLC_VAR_INTEGER );

    var_SetInteger( obj->obj.libvlc, uiManagerVariableName, reinterpret_cast< int64_t >( obj ) );
}

int tiril::extension::activate( extensions_manager_t* mgr, const std::string& script )
{
    if ( mgr != 0 )
    {
        for ( auto i = 0; i != mgr->extensions.i_size; ++i )
        {
            auto file = boost::filesystem::path( mgr->extensions.p_elems[i]->psz_name ).filename( );
            if (  file == script )
                return extension_Activate( mgr, mgr->extensions.p_elems[i] );
        }
    }
    return -1;
}

int tiril::extension::deactivate( extensions_manager_t* mgr, const std::string& script )
{
    if ( mgr != 0 )
    {
        for ( auto i = 0; i != mgr->extensions.i_size; ++i )
        {
            auto file = boost::filesystem::path( mgr->extensions.p_elems[i]->psz_name ).filename( );
            if ( file == script )
                return extension_Deactivate( mgr, mgr->extensions.p_elems[i] );
        }
    }
    return -1;
}

/*[[nodiscard]]*/ bool tiril::extension::var_string( vlc_object_t* any, const std::string& name, const std::string& val )
{
    if ( auto mgr = tiril::extension::manager( any ); mgr != 0 )
    {
        if ( var_GetInteger( mgr->obj.libvlc, name.c_str() ) == 0 )
            var_Create( mgr->obj.libvlc, name.c_str(), VLC_VAR_STRING );

        var_SetString( mgr->obj.libvlc, name.c_str( ), val.c_str() );
        return true;
    }
    return false;
}
