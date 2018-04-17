#include "tiril.h"
#include "vlc_helper.h"
#include <mutex>

char* mY_hACkY_wACky_StRDuP( const char* str )
{
    // ������ ������� text_segment_New ��������� �-������ � ������� strdup � ��������� ��������� ������ �������� � psz_text. 
    // �������� ������, �������� ��������� � "����������" �������� �������.
    tiril::scoped_do< text_segment_t > unneeded_( text_segment_New( str ), [] ( text_segment_t* p )
    {
        text_segment_Delete( p );
    } );

    char*  ptr = unneeded_->psz_text;
    unneeded_->psz_text = 0;
    return ptr;
}