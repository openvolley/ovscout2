// keypress handling
$(document).on('keydown', function (e) {
    var el = document.activeElement;
    var len = -1;
    if (typeof el.value != 'undefined') { len = el.value.length; };
    //Shiny.setInputValue('controlkey', e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.which + '@' + el.className + '@' + el.id + '@' + el.selectionStart + '@' + len + '@' + new Date().getTime(), {priority: 'event'});
    var charcode = (e.key.length === 1) ? e.key.charCodeAt(0) : '';
    Shiny.setInputValue('controlkey', e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.key + '|' + charcode + '@' + el.className + '@' + el.id + '@' + el.selectionStart + '@' + len + '@' + new Date().getTime(), {priority: 'event'});
    if (e.keyCode == 13) { e.stopPropagation(); e.preventDefault(); }
});

$(document).on('keyup', function (e) {
    var el = document.activeElement;
    var len = -1;
    if (typeof el.value != 'undefined') { len = el.value.length; };
    var charcode = (e.key.length === 1) ? e.key.charCodeAt(0) : '';
    Shiny.setInputValue('controlkeyup', e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.key + '|' + charcode + '@' + el.className + '@' + el.id + '@' + el.selectionStart + '@' + len + '@' + new Date().getTime(), {priority: 'event'});
    //Shiny.setInputValue('controlkeyup', e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.which + '@' + el.className + '@' + el.id + '@' + el.selectionStart + '@' + len + '@' + new Date().getTime(), {priority: 'event'});
});

function dbnc(f, delay) {
    let timer = 0;
    return function(...args) { clearTimeout(timer); timer = setTimeout(() => f.apply(this, args), delay); }
}
