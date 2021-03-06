﻿#pragma once

#include <atomic>
#include <memory>
#include <functional>
#include <vlc_helper.h>

namespace tiril
{
    template < typename T >
    using scoped_do = std::unique_ptr< T, std::function< void( T* ) > >;

    // Jeg synes at "template polymorphism" ser koseligere ut enn å konvertere vlc_object_t* til filter_t* hver gang
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

        static std::string var( vlc_object_t* any, const std::string& name );
        static void var( decoder_t* any, const std::string& name, const std::string& val );
    };

    struct subpic
    {
        static subpicture_t* current( );
        static void current( subpicture_t* pic );

        // Den oprinnelige til VLC er altfor lang og merkelig. Vi har gjort det kortere
        static void redraw( subpicture_t* pic );

        // SRTs funksjoner som oppdaterer texter er ikke trådstrygge altså vi handler den saken på en ellers måte
        static void redraw_srt( subpicture_t* pic );

    private:
        // A subpicture has pointers within and std::atomic doesn't help us much, just guarantees us atomic get/store. 
        // To prevent a deletion of the subpicture by VLC one must take additional measures
        static std::atomic< subpicture_t* >  subpic_;
    };

    // På grunn av mangel på den lovlige måten å få "extension manager",
    // må vi jukse og late å være noe annet for å få denne informasjonen
    struct extension
    {
        static void intf( intf_thread_t* save );
        // Ikke prøv å tilkalle disse metodene før du ikke har kalt til 'intf'
        // TODO: refactor it to bring this hidden requirement out to the type system
        static extensions_manager_t* manager( vlc_object_t* any );
/*
        // Could be useful in future
        static input_thread_t* input( vlc_object_t* any );
        // es == audio-es | spu-es 
        static std::string getInputLang( vlc_object_t* any, const std::string& es );
*/
        static bool var_string( vlc_object_t* any, const std::string& name, const std::string& val );

        // Runs one of the script from the VLC extensions folder. Case sensitive
        static int activate( extensions_manager_t* mgr, const std::string& script );
        static int deactivate( extensions_manager_t* mgr, const std::string& script );
    };
}
