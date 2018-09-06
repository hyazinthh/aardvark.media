var fst = '.first';
var snd = '.second';
var transition = 'horizontal flip';
var duration = '100ms';

function isAnimating(btn) {
	return btn.find(fst).transition('is animating') ||
		   btn.find(snd).transition('is animating');
}

function isVisible(elem) {
	return elem.is(':visible');
}

function isHovered(elem) {
    return $('#' + elem.attr('id') + ':hover').length > 0;
}

function resetButton(btn) {
	btn.find(fst).transition('stop all').transition('show');
	btn.find(snd).transition('stop all').transition('hide');
}

function onButtonHover(btn, from, to, hover) {
	if (isAnimating(btn) || !isVisible(btn) || isVisible(btn.find(to))) {
		return;
	}
	
	btn.find(from).transition(transition, duration, function() {
		if (!isVisible(btn)) {
			return; 
		}

		btn.find(to).transition(transition, duration, function () {
            if (isHovered(btn) !== hover) { onButtonHover(btn, to, from, !hover); }
		});
	});
}

function disableClickPropagation(elem) {
    elem.click( function(e) { e.stopPropagation(); } )   
}

function initAddButton(btn) {
    btn.hover( function() { onButtonHover(btn, fst, snd, true) },
               function() { onButtonHover(btn, snd, fst, false) } );                   
}

function isExpanded(frame) {
	return frame.hasClass("expanded");
}

function initPreviewFrame(frame) {
	frame.click( function () {
		if (!isExpanded(frame)) {
			frame.addClass('expanded')
		} else {
			frame.removeClass('expanded');
			resetButton(frame.find('.add.button'));
		}
	});

	frame.mouseleave( function () {
		frame.removeClass('expanded');
		resetButton(frame.find('.add.button'));
	});
}