#pragma once

#include <atomic>
#include <memory>
#include <functional>
#include <vlc_helper.h>

namespace tiril
{
    template < typename T >
    using scoped_do = std::unique_ptr< T, std::function< void( T* ) > >;

    // Jeg synes at "template polymorphism" er koseligere enn til å konvertere vlc_object_t* til filter_t* eller decoder_t* hver gang
    template< typename T >
    struct module
    {
        typedef T base_type;

        static int open( base_type* );
        static void close( base_type* );
    };
    
    // Unfortunately VLC doesn't propagate further any information regarding the codecs used by decoders.
    // We had to cheat and pretend to have become a legitimate decoder in order to intercept and preserve that info.
    // Here we just store it for our filters.
    struct spu_codec
    {
        // Tá mórán "codec"aí ag VLC ar fad ach teastaíonn "SubPicture Unit" uaimid 
        typedef int64_t fourcc;

        // Знову ж таки "шаблонний поліморфізм" зручніше, оскільки інформацію щодо поточного кодеку ми зберігаємо у глобальному libvlc об'єкті,
        // доступ до якого можемо отримати з будь-якого екземпляру vlc_object_t чи типу, що його успадковує
        template< typename T >
        static fourcc current( T* any ) { return current( reinterpret_cast< vlc_object_t* >( any ) ); }

        template< typename T >
        static void current( T* any, fourcc codec ) { current( reinterpret_cast< vlc_object_t* >( any ), codec ); }

        static fourcc current( vlc_object_t* any );
        static void current( vlc_object_t* any, fourcc codec );
    };

    struct subpic
    {
        static subpicture_t* current( );
        static void current( subpicture_t* pic );

        // Den oprinnelige til VLC er altfor lang og altfor merkelig. Vi har gjort det kortere
        static void redraw( subpicture_t* pic );

        // SRTs funksjoner som oppdaterer texter er ikke trådstrygge altså vi oppfører oss her på en annen måte
        static void redrawSrt( subpicture_t* pic );

    private:
        // A subpicture has pointers within and std::atomic doesn't help us much, just guarantees us atomic get/store. 
        // To prevent a deletion of the subpicture by VLC one must take additional measures
        static std::atomic< subpicture_t* >  subpic_;
    };

    // Again due to lack of the legal way to get the extension manager, 
    // we had to cheat and pretend to be something else in order to get that access.
    struct extension
    {
        static void intf( vlc_object_t* save );
        static extensions_manager_t* manager( vlc_object_t* any );

        static bool var_string( vlc_object_t* any, const std::string& name, const std::string& val );

        // Runs one of the script from the VLC extensions folder. Case sensitive
        static int activate( extensions_manager_t* mgr, const std::string& script );
        static int deactivate( extensions_manager_t* mgr, const std::string& script );
    };
}
