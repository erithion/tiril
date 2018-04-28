
// returns whether the card has become selected according to UI logic.
// also makes all??? required UI changes.
// is called from Haskell.
function uiToggleCard(e) {
    // check if it is already selected
    isSelected = e.classList.contains("selected");

    // REMINDER: it is either a previously-chosen element or null
    // cleanup first
    var selected = $(".selected");
    selected.removeClass("selected");
    selected.children("ul").removeClass("editable");    
    selected.children("ul").each( function() {
        // don't know how to find and destroy this sortable objects quicker
        Sortable.create( this ).destroy();
    });
    selected.children("ul").removeClass("droparea");    
    
    if (!isSelected){
        e.classList.add("selected");
        $(e).children("ul").addClass("editable");
        if ($(e).children("ul")[0].childElementCount == 0)
            $(e).children("ul").addClass("droparea");
    }
    
    $( ".tiril-word-list" ).each( function() {
        Sortable.create( this, {
            group: "different",
            animation: 150
        });
    });

    
    // threepenny cannot marshal bool
    return isSelected ? 0 : 1;
}

function updateSortable(){
    $(".selected").children("ul").each( function() {
        Sortable.create( this, {
            group: {
                name: 'words',
                put: true,
                pull: false
            },
            animation: 150,
            filter: '.js-remove',
            onFilter: function (evt) {
                // adding a Drop Area under the selected word when it doesn't contain any subwords anymore
                if(evt.item.parentNode.children.length == 1)
                    $(evt.item.parentNode).addClass("droparea");    
                
                evt.item.parentNode.removeChild(evt.item);
                var id = $(evt.item).children("i").attr("data-parent-id");
                $('#' + id).append(evt.item);
            },
            onAdd: function (/**Event*/evt) {
                var itemEl = evt.item;  // dragged HTMLElement
                evt.to;    // target list
                evt.from;  // previous list
                evt.oldIndex;  // element's old index within old parent
                evt.newIndex;  // element's new index within new parent
                if (evt.to.children.length != 0 )
                    $(evt.to).removeClass("droparea");    
            }
        });
    });

    $("[class*='tiril-detail-container']").each( function() {
        Sortable.create (this, {
            group: {
                name: 'words',
                put: false,
                pull: true
            },
            animation: 150
        });
    });
}