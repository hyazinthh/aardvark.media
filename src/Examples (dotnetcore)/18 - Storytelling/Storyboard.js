// Animated button for adding slides
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

// Collapsing preview frames
function isExpanded(frame) {
	return frame.hasClass("expanded");
}

function initCollapsingFrame(frame) {
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

	setupDropEvents(frame);
}

// Drag and drop
function setupDragEvents(frame) {
	var storyboard = $('.storyboard');

	frame.attr('draggable', 'true');

	frame.on('dragstart', function (ev) {
		// Save the id of the slide that is being dragged
		var slide = $(ev.target).attr('slide');
		ev.originalEvent.dataTransfer.setData("text/plain", slide);
		ev.originalEvent.dataTransfer.dropEffect = 'move';

		// Add class to storyboard to signal that we are dragging something
		storyboard.addClass('dragging');

		// Every preview frame that is not adjacent to the dragged frame
		// is a valid drop target
		var previewFrames = storyboard.find('.preview.frame');

		previewFrames.each(function () {
			if ($(this).attr('left') !== slide && $(this).attr('right') !== slide) {
				$(this).addClass('droppable');
			}
		});

		// The last preview frame is transformed into a collapsible frame
		previewFrames.last().addClass('collapsing');
		previewFrames.last().removeClass('static');
		
	});	

	frame.on('dragend', function (ev) {
		// Remove dragging class from storyboard
		storyboard.removeClass('dragging');

		// Reset preview frames
		var previewFrames = storyboard.find('.preview.frame');
		previewFrames.last().removeClass('collapsing');
		previewFrames.last().addClass('static');

		previewFrames.each(function () {
			$(this).removeClass('droppable expanded');
		});
	});		
}

function setupDropEvents(frame) {
	var storyboard = $('.storyboard');

	// Expand frames when dragging over and valid target 
	frame.on('dragenter', function (ev) {
		if (frame.hasClass('droppable')) {
			frame.addClass('expanded');
		}
	});

	// Collapse again
	frame.on('dragleave', function (ev) {
		frame.removeClass('expanded');
	});

	frame.on('dragover', function (ev) {
		if (frame.hasClass('droppable')) {
			ev.preventDefault();
		}
	});

	frame.on('drop', function (ev) {
		if (frame.hasClass('droppable')) {
			ev.preventDefault();

			// Get id of dragged slide, as well as ids of adjacent slides
			var slide = ev.originalEvent.dataTransfer.getData("text/plain");

			var left = frame.attr('left');
			if (left === undefined) { left = ''; }

			var right = frame.attr('right');
			if (right === undefined) { right = ''; }

			// Trigger move event
			aardvark.processEvent(storyboard.attr('id'), 'onslidemove', slide, left, right);
		}
	});
}