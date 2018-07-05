//AlloyEditor.loadLanguageResources();

var editor1 = AlloyEditor.editable('editor', {
    toolbars : {
        add: {
            buttons: ['txt']
        },
        styles: {
            selections: 
            [
                {
                    name: 'text',
                    buttons: ['chapter', 'name', 'merge'],
                    test: AlloyEditor.SelectionTest.text
                }
            ]
        }
    }
});

var editor2 = AlloyEditor.editable('editor2', {
    toolbars : {
        add: {
            buttons: ['txt']
        },
        styles: {
            selections: 
            [
                {
                    name: 'text',
                    buttons: ['chapter', 'name', 'merge'],
                    test: AlloyEditor.SelectionTest.text
                }
            ]
        }
    }   
    
});

var p1= new PerfectScrollbar( "#editor", 
                                        { wheelSpeed: 2
                                        , wheelPropagation: true
                                        , minScrollbarLength: 20 
                                        } );
var p2= new PerfectScrollbar( "#editor2", 
                                        { wheelSpeed: 2
                                        , wheelPropagation: true
                                        , minScrollbarLength: 20 
                                        } );
syncscroll.reset();

$(document).ready(function () {

    $('#ccSelectForm').validate({
        errorPlacement: function(error, element) { },
        rules: {
            title: {
                required: true
            },
            author: {
                 required: true,  // <-- redundant
            }  // <-- removed trailing comma
        }
    });

    $('#ccSelectForm input').on('keyup blur', function () { // fires on every keyup & blur
        if ($('#ccSelectForm').valid()) {                   // checks form for validity
            $('button.btn').prop('disabled', false);        // enables button
        } else {
            $('button.btn').prop('disabled', 'disabled');   // disables button
        }
    });

});    


function getData() {
    return { bookTitle: document.getElementById('title').value
           , bookAuthor: document.getElementById('author').value
           , encryptResult: false
           , bookLeft: document.getElementById("editor").innerHTML
           , bookRight: document.getElementById("editor2").innerHTML };
}