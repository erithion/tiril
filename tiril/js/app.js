var isLeftPaneSortingInitialized = false;
function initializeLeftPaneSorting(){
    if (!isLeftPaneSortingInitialized){
        var arr = $(".tiril-word-list"),
            len = arr.length;
    
        for (i = 0; i < len; ++i) {
            Sortable.create( arr[i], {
                group: "different",
                animation: 150
            });
        }
        isLeftPaneSortingInitialized = true;
    }
}

// returns whether the card has become selected according to UI logic.
// also makes all??? required UI changes.
// is called from Haskell.
function uiToggleCard(e) {
    // check if it is already selected
    isCurrentSelected = e.classList.contains("word-is-selected-now");

    // Cleanup first
    var oldSubElems = $(".word-is-selected-now > .list");
    oldSubElems.removeClass("word-is-editable-now");
    oldSubElems.removeClass("show-drop-area");
    $(".word-is-selected-now").removeClass("word-is-selected-now");
    
    // Set the new state
    if (!isCurrentSelected) {
        $(e).addClass("word-is-selected-now");
        var chi = $(e).children("ul");
        chi.addClass("word-is-editable-now");
        if (chi[0].childElementCount == 0)
            chi.addClass("show-drop-area");
    }
    
    // Simple sorting within bounds of the left-pane
    initializeLeftPaneSorting();

    // threepenny cannot marshal bool
    return isCurrentSelected ? 0 : 1;
}

var wordToSortableMap = {}
function updateSortable(){
    var it = $(".word-is-selected-now .tiril-word");
    // WARNING: the object must exist. 
    // The function heavily relies on a structure of the document
    var word  = it[0].textContent || it[0].innerText;

    // Checking if we have already created this object once
    if (!(word in wordToSortableMap)){
        // We assume there is only one such list
        var chi = $(".word-is-selected-now > .list");
        // Sortable for the left pane list
        var s = Sortable.create( chi[0], {
            group: {
                // This is to make sure the "alien" words from other lists wouldn't be added to this one
                name: word,
                put: true,
                pull: false
            },
            animation: 150,
            filter: '.js-remove',
            onFilter: function (evt) {
                // adding a Drop Area under the selected word when it doesn't contain any subwords anymore
                if(evt.item.parentNode.children.length == 1)
                    $(evt.item.parentNode).addClass("show-drop-area");    
                
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
                    $(evt.to).removeClass("show-drop-area");    
            }
        });
        wordToSortableMap[word] = s;
    }
    // Sortable for all words in the right pane
    var rightPanes = $("[class*='tiril-detail-container']"),
        len = rightPanes.length;

    // It seems that loops in JS work faster than $.each()
    for (i = 0; i < len; ++i) {
        Sortable.create (rightPanes[i], {
            group: {
                // This list words on the right is only for a single word on the left
                name: word,
                put: false,
                pull: true
            },
            animation: 150
        });
    }
}