// tr - 'li'-object
function utilTranslation_GetData(tr){
    return { viewTarget : tr.getElementsByTagName("word")[0].innerHTML
           , viewLang : tr.getElementsByTagName("lang")[0].innerHTML
           , viewDict : tr.getElementsByTagName("dict")[0].innerHTML
           , viewLem : tr.getElementsByTagName("lem")[0].innerHTML 
           , viewHash : tr.getAttribute("hash")
           };
}


function initializeLeftPaneSorting(){
/*    var arr = $(".tiril-left-pane > div"),
        len = arr.length;
    
    for (i = 0; i < len; ++i) {
        Sortable.create( arr[i], {
            group: "different",
            animation: 150
        });
    }
    */
}

function initCollapsible(id){
    $('#' + id).collapsible({
        accordion: true,
        accordionUpSpeed: 400,
        accordionDownSpeed: 400,
        collapseSpeed: 400,
        contentOpen: null,
        arrowRclass: 'arrow-r',
        arrowDclass: 'arrow-d',
        animate: true
    });
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
//        var chi = $(e).children("ul");
        var chi = $(".accordion-active > ul");
//        console.log(chi);
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
function updateSortable(word){
    // Checking if we have already created this object once
    if (!(word in wordToSortableMap)){
        // We assume there is only one such list
        var chi = $(".accordion-active > .list");
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
                
                var wordElem = $(".word-is-selected-now > word");
                var word = wordElem[0].textContent || wordElem[0].innerText

//                console.log( word ); 
//                console.log( evt.item.getAttribute("hash") ); 
//                console.log( evt.item.getAttribute("translator") ); 
//                console.log( parseInt(evt.item.getAttribute("hash"), 10));
                var transl = utilTranslation_GetData( evt.item );
                deleteTranslation (word, parseInt(transl.viewHash, 10), transl.viewDict);
            },
            onAdd: function (/**Event*/evt) {
                var itemEl = evt.item;  // dragged HTMLElement
                evt.to;    // target list
                evt.from;  // previous list
                evt.oldIndex;  // element's old index within old parent
                evt.newIndex;  // element's new index within new parent
                if (evt.to.children.length != 0 )
                    $(evt.to).removeClass("show-drop-area");

                var wordElem = $(".word-is-selected-now > word");
//                console.log( wordElem ); 
                
                var word = wordElem[0].textContent || wordElem[0].innerText
                var transl = utilTranslation_GetData( evt.item );
                // adding to database
                addTranslation( word
                              , transl.viewTarget
                              , transl.viewLang
                              , transl.viewLem
                              , transl.viewDict
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
    var left_elems = $(".word-is-editable-now > li").each(function(){
        var obj = $(this).attr('hash') + $(this).attr('translator');
        arr.push(obj);
    });
        
    var right_elems = $(".tiril-right-pane .tiril-translation-word").each(function(){
        var obj = $(this).attr('hash') + $(this).attr('translator');
        if (arr.indexOf(obj) != -1)
            $(this).addClass("disabled");
    });
}

var loadingScreen = null;

function startWaiting(){
    loadingScreen = window.pleaseWait({
        logo: "",
        backgroundColor: 'rgb(192,192,192, 0.3)',
        loadingHtml: "<div class='sk-cube-grid'><div class='sk-cube sk-cube1'></div><div class='sk-cube sk-cube2'></div><div class='sk-cube sk-cube3'></div><div class='sk-cube sk-cube4'></div><div class='sk-cube sk-cube5'></div><div class='sk-cube sk-cube6'></div><div class='sk-cube sk-cube7'></div><div class='sk-cube sk-cube8'></div><div class='sk-cube sk-cube9'></div></div>"
    });
}

function endWaiting(){
    loadingScreen.finish();
}
