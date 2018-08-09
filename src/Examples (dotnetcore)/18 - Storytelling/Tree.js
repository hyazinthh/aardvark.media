var rootSvg, svgContainer, chart, linkLayer, nodeLayer, tree;
var rootCurr = null
var root = null;
var padding = 15;
var gapLevel = 75;

function initChart() {
    rootSvg = document.getElementsByClassName("rootSvg")[0];
    svgContainer = rootSvg.parentElement;

    chart = d3.select(".rootSvg");
    linkLayer = chart.append("g");
    nodeLayer = chart.append("g");

    tree = d3.tree();

    window.addEventListener("resize", redraw);
}

function getX(x) {
    return padding + x;
}

function getY(y) {
    return padding + y * (svgContainer.clientHeight - padding * 2); 
}

function getWidth(x) {
    return x + 2 * padding;
}

function diagonal(d) {
    return "M" + getX(d.x) + "," + getY(d.y)
         + "C" + getX((d.x + d.parent.x) / 2) + "," + getY(d.y)
         + " " + getX((d.x + d.parent.x) / 2) + "," + getY(d.parent.y)
         + " " + getX(d.parent.x) + "," + getY(d.parent.y);
}

function diagonalInit(d) {
    var p = getPreviousPos(d.parent);

    return "M" + getX(p.x) + "," + getY(p.y)
         + "C" + getX(p.x) + "," + getY(p.y)
         + " " + getX(p.x) + "," + getY(p.y)
         + " " + getX(p.x) + "," + getY(p.y);
}

function translate(d) {
    return "translate(" + getX(d.x) + "," + getY(d.y) + ")";
}

function translateInit(d) {
    if (d.parent !== null) {
        var p = getPreviousPos(d.parent);
        return "translate(" + getX(p.x) + "," + getY(p.y) + ")";
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
    return (d.data.id === json.current) ? "10" : "8";
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
    root = tree(nodes);

    // Swap x / y and make gap between levels constant
    // Set width for svg element accordingly
    root.descendants().forEach(function (d) {
        d.y = d.x;
        d.x = d.depth * gapLevel;
        rootSvg.style.width = getWidth(d.x) + "px";
    });

    // Draw tree
    redraw();

    // Save the data so we can find the previous position of the parent of a new node
    rootCurr = root;
}

function redraw() {
    if (root === null) {
        return;
    }

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
}

function click(d) {
    aardvark.processEvent(document.body.id, 'onnodeclick', d.data.id);
}
