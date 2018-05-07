function initializeLeftPaneSorting(){
    var arr = $(".tiril-left-pane"),
        len = arr.length;
    
    for (i = 0; i < len; ++i) {
        Sortable.create( arr[i], {
            group: "different",
            animation: 150
        });
    }
}

var addTranslation = null;
var deleteTranslation = null;

function initApi(addTranslationFunction, deleteTranslationFunction) {
    addTranslation = addTranslationFunction;
    deleteTranslation = deleteTranslationFunction;
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

                var obj = $(evt.item).attr('hash') + $(evt.item).attr('translator'),
                    found = false;
                $(".tiril-right-pane .tiril-translation-word").each(function(){
                    if ($(this).attr('hash') + $(this).attr('translator') == obj){
                        found = true;
                        $(this).removeClass("disabled");
                    }
                });

                if(!found)
                    $('#' + id).append(evt.item);
                
                var wordElem = evt.from.parentNode.querySelectorAll("div > word");
                var word = wordElem[0].textContent || wordElem[0].innerText

//                console.log( word ); 
//                console.log( evt.item.getAttribute("hash") ); 
//                console.log( evt.item.getAttribute("translator") ); 
//                console.log( parseInt(evt.item.getAttribute("hash"), 10));
                deleteTranslation (word, parseInt(evt.item.getAttribute("hash"), 10), evt.item.getAttribute("translator"));
            },
            onAdd: function (/**Event*/evt) {
                var itemEl = evt.item;  // dragged HTMLElement
                evt.to;    // target list
                evt.from;  // previous list
                evt.oldIndex;  // element's old index within old parent
                evt.newIndex;  // element's new index within new parent
                if (evt.to.children.length != 0 )
                    $(evt.to).removeClass("show-drop-area");

                var wordElem = evt.to.parentNode.querySelectorAll("div > word");
//                console.log( wordElem ); 
                
                var word = wordElem[0].textContent || wordElem[0].innerText
                // adding to database
                addTranslation( word
                              , evt.item.getAttribute("translation")
                              , evt.item.getAttribute("lang")
                              , evt.item.getAttribute("verb")
                              , evt.item.getAttribute("translator") 
                              );
//                console.log( evt.item ); 
//                console.log( evt.from ); 
//                console.log( evt.to ); 
            }
        });
//        wordToSortableMap[word] = s;
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
            filter: ".disabled",
            animation: 150
        });
    }
    
    // disabling the same hashes and sources
    var arr = [];
    var left_elems = $(".tiril-left-pane .word-is-selected-now .list .tiril-translation-word").each(function(){
        var obj = $(this).attr('hash') + $(this).attr('translator');
        arr.push(obj);
    });
        
    var right_elems = $(".tiril-right-pane .tiril-translation-word").each(function(){
        var obj = $(this).attr('hash') + $(this).attr('translator');
        if (arr.indexOf(obj) != -1)
            $(this).addClass("disabled");
    });
}