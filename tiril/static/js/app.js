$(document).foundation()

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

// disable text-selection
function disableSelect(e) {
    return false;
}
document.onselectstart = new Function ()
document.onmousedown = disableSelect

// is called from Haskell to remove indentation in cards
function resetCardsIndent() {
    $("div").removeClass("selected")
}