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
var key_map = {
    // these are set from the shiny server on startup
    //"false|false|true|false|A": "!"
};
function mapkey(ev) {
    var ckey = ev.ctrlKey + "|" + ev.altKey + "|" + ev.shiftKey + "|" + ev.metaKey + "|" + ev.key.toLowerCase();
    return key_map[ckey];
};
var shortcut_map = {
    // these are set from the shiny server on startup
//    "true|false|false|false|a": "undo",
//    "true|false|false|false|ArrowLeft": "assign_point_left",
//    "true|false|false|false|ArrowRight": "assign_point_right"
};
function mapshortcut(ev) {
    var ckey = ev.ctrlKey + "|" + ev.altKey + "|" + ev.shiftKey + "|" + ev.metaKey + "|" + ev.key.toLowerCase();
    return shortcut_map[ckey];
};
var scoutin = [];
$(document).on("shiny:sessioninitialized", function() {
    $("#scout_in").on("keydown", function(e) {
        console.log("key:" + e.ctrlKey + "|" + e.altKey + "|" + e.shiftKey + "|" + e.metaKey + "|" + e.key);
        var newchar = mapkey(e);
        // log each character entered into #scout_in along with their corresponding clock and video times
        var vt = vidplayer ? vidplayer.currentTime() : "";
        if (newchar) {
            insertTextAtCursor(newchar);
            scoutin.push({ "key":newchar, "time":new Date().getTime(), "video_time":vt });
            Shiny.setInputValue("scout_input", scoutin);
            return false;
        }
	newchar = mapshortcut(e);
	if (newchar) {
	    Shiny.setInputValue("scout_shortcut", newchar, { priority: "event" });
	    return false;
	}
	scoutin.push({ "key":e.key, "time":new Date().getTime(), "video_time":vt });
        Shiny.setInputValue("scout_input", scoutin);
    })
});
