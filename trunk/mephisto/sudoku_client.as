// -*- java -*-

var xml_socket_box = [];

initialize (this.board_mc,
	    this.connect_dialog_mc, xml_socket_box);

function initialize (base_mc, dlg_mc, sockbox) {
    dlg_mc.connect_btn.onRelease = function () {
	var port = dlg_mc.port_text.text;
	var host = dlg_mc.hostname_text.text;
	dlg_mc._visible = false;

	trace ([host, port].join (":"));
	connect (host, port, sockbox);
	var board = create_board (base_mc, sockbox);
    };
}

function create_board (base_mc, sockbox) {
    var dest = [];
    for (var i = 0; i < 81; i ++) {
	var x = i % 9;
	var y = (i - x) / 9;
	var mc = base_mc.createEmptyMovieClip ("cell" + i, base_mc.getNextHighestDepth ());

	trace (mc);

	mc._x = x * 40;
	mc._y = y * 40;
	mc.attachMovie ("cell", "body_mc", 1);

	mc.body_mc.value_text.text = "";

	mc.onRelease = mk_onRelease (mc, base_mc, i, sockbox);

	dest[i] = mc;
    }
    return dest;
}

function mk_onRelease (cell_mc, base_mc, index, sockbox) {
    return function () {
	trace (index);

	var sel_mc = base_mc.createEmptyMovieClip ("combo" + index,
						   base_mc.getNextHighestDepth ());
	sel_mc.attachMovie ("value_selector", "mc", 1);
	sel_mc._x = cell_mc._x;
	sel_mc._y = cell_mc._y;

	var hndl_change = function (ev) {
	    trace ("change: " + ev);
	    var data = ev.target.selectedItem.data;
	    trace ("change: data: " + data);
	    if (data != "") {
		cell_mc.body_mc.value_text.text = data;
		for (var i = 0; i < 9; i ++) {
		    cell_mc.body_mc["p" + (i + 1)]._visible = false;
		}
		send_assign (index, ev.target.selectedItem.data, sockbox);
	    }
	};

	var hndl_close = function (ev) {
	    sel_mc.removeMovieClip ();
	};

	sel_mc.onEnterFrame = function () {
	    trace (sel_mc);
	    trace (sel_mc.mc);
	    trace (sel_mc.mc.cbox);
	    if (sel_mc.mc.cbox.addEventListener != null) {
		sel_mc.mc.cbox.addEventListener ("change", {change: hndl_change});
		sel_mc.mc.cbox.addEventListener ("close", {close: hndl_close});
		sel_mc.mc.cbox.open ();
		delete this.onEnterFrame;
	    }
	};
    };
}

function connect (host, port, sockbox) {
    var sock = new XMLSocket;
    sockbox[0] = sock;

    sock.onClose = function () {
	handle[0] = null;
    };

    sock.onConnect = function (suc) {
	trace ("connect!!");
    };

    sock.onXML = function (obj) {

	trace ("onXML: " + obj);
    };

    sock.connect (host, port);
}

function send_assign (index, val, sockbox) {
    var sock = sockbox[0];
    if (sock == null) {
	trace ("not connected yet.");
	return;
    }

    var msg = "<assign index=\"" + index + "\">" + val + "</assign>";

    sock.send (msg);
}
