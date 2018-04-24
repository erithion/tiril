// default config for Dragula
var drake = dragula({
  copy: false,
  copySortSource: true,
  // to drag only "draggable" classes
  moves: function (el, container, handle) {
      return handle.classList.contains('draggable');
  },
  // any dragula-container class can be dragged
  isContainer: function (el) {
      return el.classList.contains('dragula-container');
  },
  revertOnSpill: true
});

// disable text-selection. Probably won't be needed anymore
//document.onselectstart = new Function ()
//document.onmousedown = function (e) {
//    return false;
//}

// returns whether the card has become selected according to UI logic.
// also makes all??? required UI changes.
// is called from Haskell.
function uiToggleCard(e) {
    // check if it is already selected
    isSelected = e.classList.contains("selected");

    // cleanup first
    $(".selected").removeClass("selected");    

    if (!isSelected){
        e.classList.add("selected");
    }
    // threepenny cannot marshal bool
    return isSelected ? 0 : 1;
}