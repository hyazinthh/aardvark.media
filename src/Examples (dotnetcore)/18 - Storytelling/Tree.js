var chart, linkLayer, nodeLayer, tree, rootCurr = null;

function initChart() {
    chart = d3.select(".rootSvg")
        .append("g")
        .attr("transform", "translate(100, 10)");

    linkLayer = chart.append("g");
    nodeLayer = chart.append("g");

    tree = d3.tree()
        .size([200, 200]);
}

function diagonal(d) {
    return "M" + d.y + "," + d.x
        + "C" + (d.y + d.parent.y) / 2 + "," + d.x
        + " " + (d.y + d.parent.y) / 2 + "," + d.parent.x
        + " " + d.parent.y + "," + d.parent.x;
}

function diagonalInit(d) {
    var p = getPreviousPos(d.parent);

    return "M" + p.y + "," + p.x
        + "C" + p.y + "," + p.x
        + " " + p.y + "," + p.x
        + " " + p.y + "," + p.x;
}

function translate(d) {
    return "translate(" + d.y + "," + d.x + ")";
}

function translateInit(d) {
    if (d.parent !== null) {
        var p = getPreviousPos(d.parent);
        return "translate(" + p.y + "," + p.x + ")";
    } else {
        return translate(d);
    }
}

function key(node) {
    return node.data.id;
}

function fill(d) {
    return (d.data.id === json.current) ? "palegreen" : "#fff";
}

function radius(d) {
    return (d.data.id === json.current) ? 10 : 8;
}

function getPreviousPos(d) {
    function rec(d, n) {
        if (n.data.id === d.data.id) {
            return { x: n.x, y: n.y };
        }

        if (typeof n.children !== 'undefined') {
            for (var i = 0; i < n.children.length; i++) {
                var p = rec(d, n.children[i]);

                if (p !== null) {
                    return p;
                }
            }
        }

        return null;
    }

    if (rootCurr !== null) {
        return rec(d, rootCurr);
    } else {
        return { x: d.x, y: d.y };
    }
}

function update(data) {
    // Parse json string
    try {
        json = JSON.parse(data);
    } catch (err) {
        console.error(err.message + ": '" + data + "'");
        return;
    }

    // Create tree hierarchy
    var nodes = d3.hierarchy(json.tree);
    var root = tree(nodes);

    // Update, add and remove links
    var link = linkLayer.selectAll(".link")
        .data(root.descendants().slice(1), key)

    var linkEnter =
        link.enter()
            .append("path")
                .attr("class", "link")
                .attr("d", diagonalInit);

    linkEnter.transition()
        .attr("d", diagonal);

    link.exit().remove();

    link.transition()
        .attr("d", diagonal);

    // Update, add and remove nodes
    var node = nodeLayer.selectAll(".node")
        .data(root.descendants(), key)

    var nodeEnter =
        node.enter()
            .append("g")
                .attr("class", "node")
                .attr("transform", translateInit)
                .on("click", click)

    nodeEnter
        .append("circle")
            .attr("r", radius)
            .attr("fill", fill);

    nodeEnter
        .append("text")
            .attr("text-anchor", "middle")
            .attr("dominant-baseline", "central")
            .text(function (d) {
                return d.data.msg;
        });

    nodeEnter.transition()
        .attr("transform", translate);

    node.exit().remove();

    node.transition()
        .attr("transform", translate);

    node.selectAll("circle")
        .transition()
            .ease(d3.easeElastic.amplitude(2))
            .duration(1000)
                .attr("r", radius)
                .attr("fill", fill);

    // Save the data so we can find the previous position of the parent of a new node
    rootCurr = root;
}

function click(d) {
    aardvark.processEvent(document.body.id, 'onnodeclick', d.data.id);
}
