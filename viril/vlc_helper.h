#pragma once

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

// get rid of vlc_fs.h(243): error C2061: syntax error: identifier 'mode_t'
typedef int mode_t;
#define __PLUGIN__
// external to emulated poll
int poll(struct pollfd *fds, unsigned nfds, int timeout);
// this is to get rid of various errors due to the missing ssize_t type 
#if defined(_MSC_VER) 
#include <BaseTsd.h> 
typedef SSIZE_T ssize_t;
#endif
// this is to get rid of the error C3861: 'N_': identifier not found 
#define N_(str) (str) 

#include <cstddef>
// Let us hope for the best :)
#pragma warning( disable : 4244 ) 
#include <vlc_common.h>
//#include <vlc_objects.h>
#include <vlc_plugin.h>
#include <vlc_filter.h>
#include <vlc_block.h>
#include <vlc_fs.h>
#include <vlc_strings.h>
#include <vlc_subpicture.h>
#include <vlc_actions.h>
#include <vlc_mouse.h>
#include <vlc_interface.h>
#include <vlc_codec.h>
#include <vlc_fourcc.h>
#include <vlc_modules.h>
#include <vlc_dialog.h>
//#include <vlc_extensions.h>
//#include <include/vlc_spu.h>
#include <libvlc.h>

#pragma warning( default : 4244 ) 

#include <libass/ass_types.h>

#define MODULE_STRING "viril"

// Det som ligger nedenfor er fra VLC SDK

// SRT typer
struct subpicture_updater_sys_region_t
{
    struct
    {
        float x;
        float y;
    } origin, extent;
    int flags;
    int align;
    int inner_align;
    text_style_t *p_region_style;
    text_segment_t *p_segments;
    subpicture_updater_sys_region_t *p_next;
};

struct subpicture_updater_sys_t {

    subpicture_updater_sys_region_t region;
    text_style_t *p_default_style;
    float margin_ratio;
    mtime_t i_next_update;
    bool b_blink_even;
};

// ASS typer
struct decoder_sys_t
{
    mtime_t        i_max_stop;
    vlc_mutex_t    lock;
    int            i_refcount;
    struct ASS_Library    *p_library;
    struct ASS_Renderer   *p_renderer;
    video_format_t fmt;
    ASS_Track      *p_track;
};

struct subpicture_updater_sys_t2
{
    decoder_sys_t *p_dec_sys;
    char          *p_subs_data;
    int           i_subs_len;
    mtime_t       i_pts;
    struct ASS_Image     *p_img;
};

// lua
typedef struct {
    long long ll;
    long double ld;
} max_align2_t;

struct vlc_object_internals_t
{
    alignas ( max_align_t )
    char           *psz_name;
    void           *var_root;
    vlc_mutex_t     var_lock;
    vlc_cond_t      var_wait;
    unsigned int refs;
    void* pf_destructor;
    vlc_object_internals_t *next;
    vlc_object_internals_t *prev;
    vlc_object_internals_t *first;
    vlc_mutex_t tree_lock;
    struct vlc_res *resources;
};

// extensions
typedef struct extension_t {
    /* Below, (ro) means read-only for the GUI */
    char *psz_name;           /**< Real name of the extension (ro) */

    char *psz_title;          /**< Display title (ro) */
    char *psz_author;         /**< Author of the extension (ro) */
    char *psz_version;        /**< Version (ro) */
    char *psz_url;            /**< A URL to the official page (ro) */
    char *psz_description;    /**< Full description (ro) */
    char *psz_shortdescription; /**< Short description (eg. 1 line)  (ro) */
    char *p_icondata;         /**< Embedded data for the icon (ro) */
    int   i_icondata_size;    /**< Size of that data */

    struct extension_sys_t *p_sys;   /**< Reserved for the manager module */
} extension_t;

struct extensions_manager_t
{
    struct vlc_common_members obj;

    module_t *p_module;                /**< Extensions manager module */
    struct extensions_manager_sys_t *p_sys;   /**< Reserved for the module */

    struct {
            int i_alloc;
            int i_size;
            extension_t** p_elems;
    }extensions; /**< Array of extension descriptors */
    vlc_mutex_t lock;                  /**< A lock for the extensions array */

                                       /** Control, see extension_Control */
    int( *pf_control ) ( extensions_manager_t*, int, va_list );
};

enum
{
    /* Control extensions */
    EXTENSION_ACTIVATE,       /**< arg1: extension_t* */
    EXTENSION_DEACTIVATE,     /**< arg1: extension_t* */
    EXTENSION_IS_ACTIVATED,   /**< arg1: extension_t*, arg2: bool* */
    EXTENSION_HAS_MENU,       /**< arg1: extension_t* */
    EXTENSION_GET_MENU,       /**< arg1: extension_t*, arg2: char***, arg3: uint16_t** */
    EXTENSION_TRIGGER_ONLY,   /**< arg1: extension_t*, arg2: bool* */
    EXTENSION_TRIGGER,        /**< arg1: extension_t* */
    EXTENSION_TRIGGER_MENU,   /**< arg1: extension_t*, int (uint16_t) */
    EXTENSION_SET_INPUT,      /**< arg1: extension_t*, arg2 (input_thread_t*) */
    EXTENSION_PLAYING_CHANGED, /**< arg1: extension_t*, arg2 int( playing status ) */
    EXTENSION_META_CHANGED,   /**< arg1: extension_t*, arg2 (input_item_t*) */
};

#define extension_Activate( mgr, ext ) \
        extension_Control( mgr, EXTENSION_ACTIVATE, ext )

#define extension_Deactivate( mgr, ext ) \
        extension_Control( mgr, EXTENSION_DEACTIVATE, ext )

static inline int extension_Control( extensions_manager_t *p_mgr,
                                     int i_control, ... )
{
    va_list args;
    va_start( args, i_control );
    int i_ret = p_mgr->pf_control( p_mgr, i_control, args );
    va_end( args );
    return i_ret;
}


// Розробники, нажаль, не залишили відкритих функцій виділення пам'яті.
// Оскільки плагін і VLC обидва використовують різні CRT (першій - новий MSVC2017 UCRT, тоді як VLC - стару MSVCRT.DLL, на яку посилається MinGW при збірці),
// се врешті призводить до крешів, якщо VLC намагається висвободити пам'ять що було виділено плагіном.
// Ся функція вирішує складність - виділяє пам'ять у кучі VLC і водночас дублює С-строку. Може внутрощі виглядають дивно, але працює.
// Перехресна-компіляція мала б вирішити зазначену проблему, але на момент впровадження виникли складнощі у MinGW збірці під Debian (MinGW bug in CRT: unresolved symbol).
// Збірка зі Студії 2017 компілером MinGW з під MSYS2 теж могла б спрацюівати абиж тількі розробники залишили нормальні libvlc.a/libvlccore.a замість .la файлів від LibTool.
// TODO: Remove the hack as soon as the cross-compilation on Linux will have become possible in favor of the native CRT's malloc/strdup.
// Mind you, there exists libvlc_free, though. It can be used to free the memory, allocated this way at will.
char* mY_hACkY_wACky_StRDuP( const char* str );