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

// https://stackoverflow.com/questions/1125292/how-to-move-the-cursor-to-the-end-of-a-contenteditable-entity
function setEndOfContenteditable(elem) {
    let sel = window.getSelection();
    sel.selectAllChildren(elem);
    sel.collapseToEnd();
}
// https://stackoverflow.com/questions/2940882/need-to-set-cursor-position-to-the-end-of-a-contenteditable-div-issue-with-sele/2943242#2943242
function insertTextAtCursor(text) {
    var sel, range, textNode;
    if (window.getSelection) {
        sel = window.getSelection();
        if (sel.getRangeAt && sel.rangeCount) {
            range = sel.getRangeAt(0);
            range.deleteContents();
            textNode = document.createTextNode(text);
            range.insertNode(textNode);
            range.setStart(textNode, textNode.length); // move cursor to the end of the newly inserted text node
            range.setEnd(textNode, textNode.length);
            sel.removeAllRanges();
            sel.addRange(range);
        }
    } else if (document.selection && document.selection.createRange) {
        range = document.selection.createRange();
        range.pasteHTML(text);
    }
};

// scouting key remapping, to make input from one key give different text in the input box
var sk_key_map = {}; // set from the shiny server on startup
function sk_mapkey(ev) {
    var ckey = ev.ctrlKey + "|" + ev.altKey + "|" + ev.shiftKey + "|" + ev.metaKey + "|" + ev.key.toLowerCase();
    console.log("ckey: " + ckey);
    return sk_key_map[ckey];
};

// scouting shortcuts, so that key combinations in the input box can trigger actions in the server code
var sk_shortcut_map = {}; // set from the shiny server on startup
function sk_mapshortcut(ev) {
    var ckey = ev.ctrlKey + "|" + ev.altKey + "|" + ev.shiftKey + "|" + ev.metaKey + "|" + ev.key.toLowerCase();
    return sk_shortcut_map[ckey];
};

var scoutin = [];
var scout_in_el;
$(document).on("shiny:sessioninitialized", function() {
    scout_in_el = $("#scout_in");
});

function sk_handler(e) {
    var newchar = sk_mapkey(e);
    console.log(" -- maps to scout key: " + newchar);
    // log each character entered into #scout_in along with their corresponding clock and video times
    var vt;
    if (typeof vidplayer !== "undefined") { vt = vidplayer.currentTime(); } else { vt = ""; }
    var d = new Date();
    var dloc = d.getTime() - d.getTimezoneOffset() * 60 * 1000;
    // time is returned in local time, if we want UTC then use just d.getTime()
    if (newchar) {
        insertTextAtCursor(newchar);
        scoutin.push({ "key":newchar, "time":dloc, "video_time":vt });
        Shiny.setInputValue("scout_input_times", scoutin);
        return false;
    }
    newchar = sk_mapshortcut(e);
    console.log(" -- maps to scout shortcut: " + newchar);
    if (newchar) {
        Shiny.setInputValue("scout_shortcut", newchar, { priority: "event" });
        return false;
    }
    scoutin.push({ "key":e.key, "time":dloc, "video_time":vt });
    Shiny.setInputValue("scout_input_times", scoutin);
}

function plk_handler(e) {
    // arrow up/down in plays table is handled in the server code
    if ((e.keyCode == 38 || e.keyCode == 40)) { e.stopPropagation(); e.preventDefault(); }
    // tab switches focus to scout input bar. TODO make this a configurable shortcut
    if (e.keyCode == 9) { var el = document.getElementById('scout_in'); if (el) { el.focus(); e.stopPropagation(); e.preventDefault(); } }
}

$(document).on('keydown', function (e) {
    var el = document.activeElement;
    var len = -1;
    if (typeof el.value != 'undefined') { len = el.value.length; };
    var charcode = (e.key.length === 1) ? e.key.charCodeAt(0) : '';
    Shiny.setInputValue('controlkey', e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.key + '|' + charcode + '@' + el.className + '@' + el.id + '@' + el.selectionStart + '@' + len + '@' + new Date().getTime(), {priority: 'event'});
    if (e.key === "Enter") { e.stopPropagation(); e.preventDefault(); }
    if (el.id.includes("scout_in")) {
        if (e.key === "Enter") {
            // send the actual text in the box to the Shiny server
            Shiny.setInputValue("scout_input", scout_in_el.text(), { priority: "event" });
            scout_in_el.text(""); // clear it
            scoutin = [];
            Shiny.setInputValue("scout_input_times", scoutin);
        }
    } else if (el.className.includes("pl2_fixhdr")) {
        // send this event to the playslist input handler
        return plk_handler(e);
    } else if (el.id.includes("scout_in")) {
        // send this event to the scout input handler
        return sk_handler(e);
    }
});

$(document).on('keyup', function (e) {
    var el = document.activeElement;
    var len = -1;
    if (typeof el.value != 'undefined') { len = el.value.length; };
    var charcode = (e.key.length === 1) ? e.key.charCodeAt(0) : '';
    Shiny.setInputValue('controlkeyup', e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.key + '|' + charcode + '@' + el.className + '@' + el.id + '@' + el.selectionStart + '@' + len + '@' + new Date().getTime(), {priority: 'event'});
});

function dbnc(f, delay) {
    let timer = 0;
    return function(...args) { clearTimeout(timer); timer = setTimeout(() => f.apply(this, args), delay); }
}
