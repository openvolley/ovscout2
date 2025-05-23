set_vspinner = function() {
    $('#review_player').addClass('loading');
};
remove_vspinner = function() {
    $('#review_player').removeClass('loading');
};
pause_main_video_on_click = false;
$(document).on("shiny:sessioninitialized", function() {
    $('#video_overlay').click(function(e) {
        flash_screen();
        // previously this was only for if (app_data$scout_mode != "type")
        if (pause_main_video_on_click) {
            videojs.players.main_video.pause(); // or use videojs.getPlayer('main_video')
            pause_on_type = 0;
        }
        var rect = e.target.getBoundingClientRect();
        var cx = e.clientX - rect.left;
        var cy = e.clientY - rect.top;
        var vt = -1;
        try { vt = vidplayer.currentTime(); } catch(err) {};
        Shiny.setInputValue('video_click', [cx, cy, rect.width, rect.height, vt, e.shiftKey], {priority: 'event'})
    });
});

// https://stackoverflow.com/questions/7404366/how-do-i-insert-some-text-where-the-cursor-is
function insertTextAtCursor(text) {
    var el = document.getElementById("scout_in");
    var val = el.value, endIndex, range, doc = el.ownerDocument;
    if (typeof el.selectionStart == "number" && typeof el.selectionEnd == "number") {
        endIndex = el.selectionEnd;
        el.value = val.slice(0, endIndex) + text + val.slice(endIndex);
        el.selectionStart = el.selectionEnd = endIndex + text.length;
    } else if (doc.selection != "undefined" && doc.selection.createRange) {
        el.focus();
        range = doc.selection.createRange();
        range.collapse(false);
        range.text = text;
        range.select();
    }
}

// scouting key remapping, to make input from one key give different text in the input box
var sk_key_map = {}; // set from the shiny server on startup
function sk_mapkey(ev) {
    var ckey = ev.ctrlKey + "|" + ev.altKey + "|" + ev.shiftKey + "|" + ev.metaKey + "|" + ev.key; // unclear if this should be ev.key.toLowerCase() or not TODO check/fix
    console.log("ckey: " + ckey);
    return sk_key_map[ckey];
};

// scouting shortcuts, so that key combinations in the input box can trigger actions in the server code
var sk_shortcut_map = {}; // set from the shiny server on startup
function sk_mapshortcut(ev) {
    var ckey = ev.ctrlKey + "|" + ev.altKey + "|" + ev.shiftKey + "|" + ev.metaKey + "|" + ev.key;
    return sk_shortcut_map[ckey];
};

var scoutin = [];
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
        return false; // stop propagation
    }
    newchar = sk_mapshortcut(e);
    console.log(" -- maps to scout shortcut: " + newchar);
    if (newchar) {
        Shiny.setInputValue("scout_shortcut", newchar, { priority: "event" });
        return false; // stop propagation
    }
    scoutin.push({ "key":e.key, "time":dloc, "video_time":vt });
    Shiny.setInputValue("scout_input_times", scoutin);
}

function plk_handler(e) {
    // arrow up/down 38/40 in plays table is handled in the server code
    // tab in playstable switches focus to scout input bar. Also now handled in server code. TODO make this a configurable shortcut
    if ((e.keyCode == 38 || e.keyCode == 40 || e.keyCode == 9)) { e.stopPropagation(); e.preventDefault(); }
}

var ck_shortcut_map = {}; // set from the shiny server on startup
function ck_handler(e) {
    var newchar = ck_shortcut_map[e.ctrlKey + "|" + e.altKey + "|" + e.shiftKey + "|" + e.metaKey + "|" + e.key];
    console.log(" -- maps to click shortcut: " + newchar);
    if (newchar) {
        // click-mode shortcuts are all handled in the server.R function BUT we want to stop the key event propagating further, if it's a recognized shortcut BUT not if we are e.g. editing a code or entering players, because e.g. the "a" or "q" keys might be shortcuts in click mode but we want to be able to type with them
        if ($("body.modal-open").length == 1) {
            // some modal is open
            // TODO this needs further testing
        } else {
            return false; // stop propagation
        }
    }
}

var pause_on_type = 0; // no pause
var pause_on_type_timer;
$(document).on('keydown', function (e) {
    var el = document.activeElement;
    var len = -1;
    if (typeof el.value != 'undefined') { len = el.value.length; };
    var charcode = (e.key.length === 1) ? e.key.charCodeAt(0) : '';
    Shiny.setInputValue('controlkey', e.ctrlKey + '|' + e.altKey + '|' + e.shiftKey + '|' + e.metaKey + '|' + e.key + '|' + charcode + '@' + el.className + '@' + el.id + '@' + el.selectionStart + '@' + len + '@' + new Date().getTime(), {priority: 'event'});
    if (e.key === "Enter") { e.stopPropagation(); e.preventDefault(); }
    // key events in a modal will be in the body element of class modal-open or in the modal itself
    if (el.className.includes("scedit-modal") || ($("body .scedit-modal").length == 1) ) {
        // key event into the scedit modal
        e.stopPropagation(); e.preventDefault();
//    } else if ($("body.modal-open").length == 1) {
//        // some other modal is open
    } else if (el.id.includes("scout_in")) {
        if (pause_on_type > 0) {
            vidplayer.pause();
            clearTimeout(pause_on_type_timer);
            pause_on_type_timer = setTimeout(function() { if (pause_on_type > 0) { vidplayer.play(); }}, pause_on_type);
        }
        if (e.key === "Enter") {
            // send the actual text in the box to the Shiny server
            Shiny.setInputValue("scout_input", $("#scout_in").val(), { priority: "event" });
            $("#scout_in").val(""); // clear it
            scoutin = [];
            Shiny.setInputValue("scout_input_times", scoutin);
        } else {
            // send this event to the scout input handler
            return sk_handler(e);
        }
    } else if (el.id.includes("playslist-tbl-i")) {
        // send this event to the playslist input handler
        return plk_handler(e);
    } else {
        return ck_handler(e); // send to click-mode handler
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
