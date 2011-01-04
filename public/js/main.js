$(function(){
    var lineno = 1;
    $("span.lineBreak").each(function(){
        $(this).attr("id", "L" + lineno);
        lineno += 1;
    });

    $(".lineNumber").click(function(){
        $(".lineNumber").removeClass("lineNumberActive");
        $(this).addClass("lineNumberActive");
    });

    $(".relatize").each(function(){
        $(this).attr("title", $(this).text());
        $(this).relatizeDate();
    });

    if ($("form.dependencies").length)
        dependencyCanvas();
    
    $(".issue-tags #assign").change(function(){
        if ($(this).val() == "")
            return;

        addTag("assign:" + $(this).val());
    });
    
    $(".issue-tags #type").change(function(){
        if ($(this).val() == "")
            return;

        addTag("type:" + $(this).val());
    });

    $(".issue-tags .kill").live("click", function(){
        $(this).parent().remove();
    });

    $(".issue-comment, .issue-body").submit(function(){
        var tags = $(".issue-tags li").map(function(i, v){
            return $(v).find("a:nth(1)").text();
        }).toArray();

        console.log(tags);

        $("input#tags").val(tags.join(", "));
    });

    $("#add-tag").submit(function(){
        console.log(this, $(this));
        addTag($("#add-tag #tag-name").val());
        $("#add-tag #tag-name").val("");
        return false;
    });

    $("form.issue-comment .revise-fields").hide();

    $("form.issue-comment h3").click(function(){
        $(this).html("&larr; close");
        $("form.issue-comment .revise-fields").slideToggle("fast", function(){
            if ($(this).css("display") == "none")
                $("form.issue-comment h3").html("revise issue &rarr;");
        });
    }).hover(function(){
        $(this).css("background", "#222");
    }, function(){
        $(this).css("background", "#151515");
    }).css({
        "background": "#151515",
        "padding": "0 .9em",
        "font-size": "1.2em",
        "color": "#fff",
        "margin-bottom": "1em",
        "-webkit-border-radius": "0.5em",
        "cursor": "pointer",
        "line-height": "2.5em"
    }).html("revise issue &rarr;");
});

function addTag(tag) {
    var url, link, kill, item;

    url =
        $(".head a:nth(2)").attr("href") + "/issues/tag/" + tag;

    link =
        $(document.createElement("a"))
            .attr("href", url)
            .text(tag);

    kill =
        $(document.createElement("a"))
            .attr("class", "kill")
            .attr("href", "javascript:void(0)");

    item = $(document.createElement("li"));
    item.append(kill);
    item.append(link);

    $(".issue-tags .tags").append(item);
}

function dependencyCanvas() {
    $(document.createElement("canvas"))
        .attr("id", "depends_canvas")
        .prependTo("form.subtle");

    $("#depends_canvas").css({
        float: "left",
        marginLeft: -300,
        marginTop: $("form.subtle h1").height() + parseInt($("form.subtle h1").css("margin-bottom"), 10),
    }).attr({
        width: 300,
        height: $(".fork-log").height()
    }); //.attr({ width: $(document).width(), height: $(document).height() });

    var canvas = document.getElementById("depends_canvas").getContext("2d");
    var dependency_displayed = [];

    $(".fork-log .change").each(function(){
        var change = $(this);

        $(this).find(":checkbox").click(function(){
            setChecked($(this), $(this).attr("checked"));
        });

        canvas.fillStyle = "#000";
        canvas.strokeStyle = "#ccc";
        canvas.lineWidth = 2;

        var depends = rowDependencies($(this));
        $(depends).each(function(i, dep){
            var depend = $("#change-"+ dep);

            var line_from_x = 300;
            var line_from_y = change.offset().top - $(".fork-log").offset().top + (change.height() / 2) - 1;

            var line_to_x = 299;
            var line_to_y = depend.offset().top - $(".fork-log").offset().top + (change.height() / 2) - 1;

            var height = line_to_y - line_from_y;
            var width = line_to_x - line_from_x;

            var median = line_from_y + (line_to_y - line_from_y);
            var height = line_to_y - line_from_y;
            var curve = line_from_x - 5 - (20 * (height / 50));

            canvas.beginPath();
            canvas.moveTo(line_from_x, line_from_y);
            canvas.quadraticCurveTo(curve, median, line_to_x, line_to_y);
            canvas.stroke();

            canvas.lineTo(line_to_x - 4, line_to_y - 4);
            canvas.moveTo(line_to_x, line_to_y);
            canvas.lineTo(line_to_x - 4, line_to_y + 4);
            canvas.stroke();
        });
    });
}

function setChecked(ele, checked) {
    ele.attr("checked", checked);
    var depends = rowDependencies(ele.closest(".change"));
    $(depends).each(function(i, dep){
        setChecked($("#change-"+ dep +" :checkbox"), checked);
    });
}

function rowDependencies(ele) {
    return ele.attr("class").split(" ").slice(1).map(function(c){
        return c.replace(/depends-on-/, "");
    });
}
