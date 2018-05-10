function utilSelectize_Clone(){
    var obj = $('selectize')[0].cloneNode(false);
    obj.setAttribute( "value", "" );
    obj.setAttribute( "tabindex", "0");
    return obj;
}
        
// appending column as last
function utilTable_AppendColumn(table) {
    for (var i = 0; i < table.rows.length; i++)
        table.rows[i].insertCell(-1);
}

// removing last column
function utilTable_RemoveColumn(table) {
    for (var i = 0; i < table.rows.length; i++)
        table.rows[i].deleteCell(-1);
}    
        
// elem - 'translation'-element
// val - true/false
function utilTranslation_SetManuallyMoved(translation, val){
    $(translation).attr("manually-moved", val.toString());
}

// tr - 'translation'-object
function utilTranslation_GetData(tr){
    return { exportTarget : tr.getElementsByTagName("word")[0].innerHTML
           , exportLang : tr.getElementsByTagName("lang")[0].innerHTML
           , exportDict : tr.getElementsByTagName("dict")[0].innerHTML
           , exportLem : tr.getElementsByTagName("lem")[0].innerHTML 
           };
}

// children - children of td which contains translations
// tags - if one of the tags is present within a child, the child will be returned with eq==true
function utilTranslation_CompareByTags(children, tags){
    var ret = [ ];
    for (var i = 0; i < children.length; ++i){
        var data = utilTranslation_GetData(children[i]),
            equal = false;
        for(var prop in data) {
            equal = equal || tags.includes(data[prop]);
        }
        ret.push({ arr: children[i], eq : equal });
    }
    return ret;
}

function rearrangeItems(parentTable, currentRowIdx, currentColIdx, newColIdx, currentTags){
    // starting from the next row
    var rowIndent = currentRowIdx + 1; // simply formality cause currentRowIdx is always 0 
    // loop assumes that row[0] contains tags; row[i] - header; row[i+1] - data
    for (var i = rowIndent; i < parentTable.rows.length; i += 2){
        var originalWord = parentTable.rows[i].cells[0].getElementsByTagName("translation-group-title")[0].innerHTML,
            newTarget = parentTable.rows[i + 1].cells[newColIdx],
            oldTarget = parentTable.rows[i + 1].cells[currentColIdx],
            pos = [];
        initSortable(originalWord, newTarget);
        
        for (var j = currentColIdx; j < parentTable.rows[rowIndent].cells.length; ++j){
            var temp = utilTranslation_CompareByTags(parentTable.rows[i + 1].cells[j].children, currentTags);
            pos = Array.prototype.concat( pos, temp );
        }
        
        for (var j = 0; j < pos.length; ++j){
            utilTranslation_SetManuallyMoved(pos[j].arr, false);
            // moving
            (pos[j].eq ? oldTarget : newTarget).appendChild(pos[j].arr);
            $(pos[j].arr).hide().fadeIn(1000);
        }
    }

    var len = parentTable.rows[0].cells.length;
    for (var i = newColIdx + 1; i < len; ++i)
        // without an index the last column is always assumed
        utilTable_RemoveColumn(parentTable);
    // items of the neighbour must be cleared if it is still one left to th right of the current one
    if (newColIdx != currentColIdx)
        $(parentTable.rows[0].cells[newColIdx]).children("selectize")[0].selectize.clear(true); // true - no change events

}

// sortingGroup - the name for the group. typically the original word from the caption
// sortableElem - DOM td-elem, containing elems for sortable. it is td from the row that follows a row with a caption
function initSortable(sortingGroup, sortableElem){
    Sortable.create( sortableElem, {
        group: sortingGroup,
        animation: 100,
        filter: "close",
        onFilter: function (evt) {
            var item = evt.item,
                ctrl = evt.target;

            if (Sortable.utils.is(ctrl, "close")) {  // Click on remove button
                item.parentNode.removeChild(item); // remove sortable item
            }
        },
        onAdd: function (/**Event*/evt) {
            // un-/marking rhe element as a manually-moved if it doesn't match any tags within the current TD
            var topTd = $(evt.to).closest('table')[0].rows[0].cells[evt.to.cellIndex];
            var tags = $(topTd).children("selectize")[0].selectize.items;
            var ret = utilTranslation_CompareByTags([evt.item], tags);
            utilTranslation_SetManuallyMoved(evt.item, !ret[0].eq);
        }
    });
}        

// assumes a tag selector has the tag-name 'selectize'
function initSelectize(){
    var tags = $('selectize');
    tags.selectize({
        theme: 'bootstrap3',
        delimiter: ',',
        persist: false,
        createOnBlur: true,
        valueField: 'value',
        labelField: 'value',
        searchField: 'value',
        plugins: ['remove_button'],
        create : true
        , onItemAdd : function(value, item){
            var parentTd = this.$input[0].parentNode,
                parentTable = $(parentTd).closest('table')[0],
                currentRowIdx = parentTd.parentNode.rowIndex,
                currentColIdx = parentTd.cellIndex,
                newColIdx = currentColIdx + 1;
            
            // If the current column is the last one, create new for words to move
            if (currentColIdx == parentTable.rows[currentRowIdx].cells.length - 1){
                utilTable_AppendColumn(parentTable);
                var newObj = utilSelectize_Clone();
                parentTable.rows[currentRowIdx].cells[newColIdx].appendChild(newObj);
                initSelectize();
            }

            rearrangeItems( parentTable, currentRowIdx, currentColIdx, newColIdx, this.items );
        }
        , onItemRemove : function (value, item){
            var parentTd = this.$input[0].parentNode,
                parentTable = $(parentTd).closest('table')[0],
                currentRowIdx = parentTd.parentNode.rowIndex,
                currentColIdx = parentTd.cellIndex,
                // there is no tags left in the current selectizer, 
                // so we must move all words into the current one and delete all the olumns beyond
                newColIdx = this.items.length == 0 ? currentColIdx : currentColIdx + 1;
                
            rearrangeItems( parentTable, currentRowIdx, currentColIdx, newColIdx, this.items );
        }
    });
}

function finalCollect(tableId) {
    // starting from the next row
    var table = document.getElementById(tableId),
        rowIndent = 1,
        dataset = [];
    // loop assumes that row[0] contains tags; row[i] - header; row[i+1] - data
    for (var i = rowIndent; i < table.rows.length; i += 2){
        var originalWord_ = table.rows[i].cells[0].getElementsByTagName("translation-group-title")[0].innerHTML,
            row = [];
        
        for (var j = 0; j < table.rows[i].cells.length; ++j){
            var col = [];
            $(table.rows[i + 1].cells[j]).children("translation").map( function( idx, obj ){
                col.push( utilTranslation_GetData( obj ) );
            });
            row.push(col);
        }
        dataset.push({ exportSource: originalWord_, exportTargets: row });
    }
    return dataset;
}

function initExport(tableId){
    var tbl = document.getElementById(tableId);
    for (var i = 1; i < tbl.rows.length; i += 2){
        var originalWord = tbl.rows[i].cells[0].getElementsByTagName("translation-group-title")[0].innerHTML;
        initSortable(originalWord, tbl.rows[i + 1].cells[0]);
    }
    initSelectize();
}

// With options (defaults shown below)
$('.toggle').toggles({
  drag: true, // allow dragging the toggle between positions
  click: true, // allow clicking on the toggle
  text: {
    on: 'Comma', // text for the ON position
    off: 'Tab' // and off
  },
  on: false, // is the toggle ON on init
  animate: 250, // animation time (ms)
  easing: 'swing', // animation transition easing function
  checkbox: null, // the checkbox to toggle (for use in forms)
  clicker: null, // element that can be clicked on to toggle. removes binding from the toggle itself (use nesting)
  width: 100, // width used if not set in css
  height: 30, // height if not set in css
  type: 'select' // if this is set to 'select' then the select style toggle will be used
});