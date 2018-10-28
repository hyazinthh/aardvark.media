var baseWidth = 1;
var baseHeight = 1;
var drag = { elem: null, prev: null };
var resize = { elem: null, prev: null };

function point(x, y) {
    return { 'X': x, 'Y': y };
}

function getScale() {
    var cont = $('.render.overlay');
    return Math.min(cont.width() / baseWidth, cont.height() / baseHeight);
}

function transform(elem) {
    var pos = elem.data('pos');

    elem.css({
        'transform': `scale(${getScale()}) translate(${pos.X}px, ${pos.Y}px)`
    });    
}

function autosize(elem) {
    elem.children('textarea')
		.height('auto')
        .height(function() {
            return `${this.scrollHeight}px`;
        });
}

function setWidth(elem, width) {
    elem.width(width);
    autosize(elem);
}

function setPosition(elem, pos) {
    elem.data('pos', pos);
    transform(elem);
}

function setTop(elem) {
    $('.top.label').each(function () {
        $(this).removeClass('top');
    });
    elem.addClass('top');
}

// Initializes the annotation for the Javascript
// side (dragging, resizing, ...)
function initLabel (elem) {
    elem.on('mouseenter', function() { if (drag.elem === null) elem.addClass('hovered'); })
        .on('mouseleave', function() { elem.removeClass('hovered'); });

    elem.children('textarea')
		.on('focus', function() { elem.addClass('focused'); })
		.on('blur', function() { elem.removeClass('focused'); })
		.on('input', function() { autosize(elem); });

    elem.children('.move.button').on('mousedown', function(ev) {
        setTop(elem);
		drag.elem = elem;
		drag.prev = point(ev.clientX, ev.clientY);
    });
	
	elem.children('.grabber').on('mousedown', function(ev) {
		resize.elem = elem;
		resize.prev = ev.clientX;
    });
}

// Resizing
function resizeLabel(ev) {
    if (resize.elem == null) {
        return;
    }
    var scale = getScale();

    var curr = ev.clientX;
    var delta = (curr - resize.prev) / scale;
    resize.prev = curr;

    var w = resize.elem.width();
    setWidth(resize.elem, Math.max(100, Math.round(w + delta)));
}

function resizeEnd() {
    var elem = resize.elem;

	if (elem !== null) {
        aardvark.processEvent(elem.attr('id'), 'onlabelresized', elem.width());
    }
    
    resize.elem = null;
}

// Dragging
function dragLabel(ev) {
    if (drag.elem == null) {
        return;
    }

    var scale = getScale();

    var curr = point(ev.clientX, ev.clientY);
    var delta = point((curr.X - drag.prev.X) / scale, (curr.Y - drag.prev.Y) / scale);
    drag.prev = curr;

    var pos = drag.elem.data('pos');
    setPosition(drag.elem, point(Math.round(pos.X + delta.X), 
                                 Math.round(pos.Y + delta.Y)));
}

function dragEnd() {
    var elem = drag.elem;

	if (elem !== null) {
        aardvark.processEvent(elem.attr('id'), 'onlabelmoved', elem.data('pos'));
    }
    
    drag.elem = null;
}

// Register event listeners
document.addEventListener('mousemove', function(ev) {
	dragLabel(ev);
	resizeLabel(ev);
});

document.addEventListener('mouseup', function() {
	dragEnd();
	resizeEnd();
});

window.addEventListener('resize', function() {
    $('.label').each(function() { 
        transform($(this)); 
    });
});