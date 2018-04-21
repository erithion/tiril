$(document).foundation()
dragula([
	document.getElementById('b1'),
	document.getElementById('b2'),
  document.getElementById('b3')
])

// Scrollable area
//var element = document.getElementById("boards"); // Count Boards
//var numberOfBoards = element.getElementsByClassName('board').length;
//var boardsWidth = numberOfBoards*316 // Width of all Boards
//console.log(boardsWidth);
//element.style.width = boardsWidth+"px"; // set Width

var drake = dragula({
  copy: false,
  copySortSource: true,
  moves: function (el, container, handle) {
    return handle.classList.contains('draggable');
  },
  isContainer: function (el) {
    return el.classList.contains('dragula-container');
  },
  revertOnSpill: true
});

//function addDragula(name) {
//    drake.containers.push(document.getElementById(name));
//}

// disable text-selection
function disableselect(e) {return false;}
document.onselectstart = new Function ()
document.onmousedown = disableselect

function resetWordsSelection(){
    $("div").removeClass("selected")
}