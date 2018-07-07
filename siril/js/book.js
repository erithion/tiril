$("#encryptId").change(function () {     
    if (!$(this).prop('checked')) {  
         $('#smartbookEncryptModal').modal();
    } 
});

$('#smartbookEncryptModal .modal-footer button').on('click', function(event) {
  var $button = $(event.target);

  $(this).closest('.modal').one('hidden.bs.modal', function() {
    if ( $button[0].id == "cancel"){
        $('#encryptId').prop('checked', true).change();
    }
  });
});

//AlloyEditor.loadLanguageResources();

var leftEditor = AlloyEditor.editable('leftEditor', {
    extraPlugins: AlloyEditor.Core.ATTRS.extraPlugins.value + ',find',
    toolbars : {
        add: {
            buttons: AlloyEditor.getButtons(['txt', 'find']),
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

var rightEditor = AlloyEditor.editable('rightEditor', {
    extraPlugins: AlloyEditor.Core.ATTRS.extraPlugins.value + ',find',
    toolbars : {
        add: {
            buttons: AlloyEditor.getButtons(['txt', 'find']),
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

var psLeft,
    psRight = null;

function createPerfectScrollbars() {
    psLeft = new PerfectScrollbar( "#leftEditor", 
                                        { wheelSpeed: 2
                                        , wheelPropagation: true
                                        , minScrollbarLength: 20 
                                        } );
    psRight = new PerfectScrollbar( "#rightEditor", 
                                        { wheelSpeed: 2
                                        , wheelPropagation: true
                                        , minScrollbarLength: 20 
                                        } );
}
syncscroll.reset();
createPerfectScrollbars();

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
            $('#save').prop('disabled', false);        // enables button
        } else {
            $('#save').prop('disabled', 'disabled');   // disables button
        }
    });

});    


function getData() {
    // making sure scrollbar's divs won't end up in innerHTML content
    psLeft.destroy();
    psRight.destroy();
    var data =
           { jsEncryptResult : $('#encryptId').prop('checked')
           , jsBook : { bookTitle: document.getElementById('title').value
                      , bookAuthor: document.getElementById('author').value
                      , bookLeft: document.getElementById("leftEditor").innerHTML
                      , bookRight: document.getElementById("rightEditor").innerHTML }
           };
    // recreate
    createPerfectScrollbars();
    return data;
}