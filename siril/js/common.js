function uninit() {
    if (typeof editor1 !== 'undefined' && editor1 !== null)
        editor1.destroy();

    if (typeof editor2 !== 'undefined' && editor2 !== null)
        editor2.destroy();
    
    editor1 = null;
    editor2 = null;
}