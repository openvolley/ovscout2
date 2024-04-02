set_vspinner = function() {
    $('#review_player').addClass('loading');
};
remove_vspinner = function() {
    $('#review_player').removeClass('loading');
};
$(document).on("shiny:sessioninitialized", function() {
    $('#video_overlay').click(function(e) {
        var rect = e.target.getBoundingClientRect();
        var cx = e.clientX - rect.left;
        var cy = e.clientY - rect.top;
        var vt = -1;
        try { vt = vidplayer.currentTime(); } catch(err) {};
        Shiny.setInputValue('video_click', [cx, cy, rect.width, rect.height, vt, e.shiftKey], {priority: 'event'})
    });
});

// keypress handling - 'keypress' is deprecated, use keydown instead
//$(document).on('keypress', function (e) {
//    var el = document.activeElement;
//    var len = -1;
//    if (typeof el.value != 'undefined') { len = el.value.length; };
//    Shiny.setInputValue('cmd', e.which + '@' + el.className + '@' + el.id + '@' + el.selectionStart + '@' + len + '@' + new Date().getTime(), {priority: 'event'});
//});

$(document).on('keydown', function (e) {
    var el = document.activeElement;
    var len = -1;
    if (typeof el.value != 'undefined') { len = el.value.length; };
    //Shiny.setInputValue('controlkey', e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.which + '@' + el.className + '@' + el.id + '@' + el.selectionStart + '@' + len + '@' + new Date().getTime(), {priority: 'event'});
    var charcode = (e.key.length === 1) ? e.key.charCodeAt(0) : '';
    Shiny.setInputValue('controlkey', e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.key + '|' + charcode + '@' + el.className + '@' + el.id + '@' + el.selectionStart + '@' + len + '@' + new Date().getTime(), {priority: 'event'});
    if (e.keyCode == 13) { e.stopPropagation(); e.preventDefault(); }
    if (el.className.includes("pl2_fixhdr")) {
        // arrow up/down in plays table is handled in the server code
        if ((e.keyCode == 38 || e.keyCode == 40)) { e.stopPropagation(); e.preventDefault(); }
        // tab switches to scout input bar
        if (e.keyCode == 9) { var el = document.getElementById('scout_in'); if (el) { el.focus(); e.stopPropagation(); e.preventDefault(); } }
    }
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
