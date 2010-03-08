$(function(){
	$(".relatize").each(function(){
		$(this).attr("title", $(this).text());
		$(this).relatizeDate();
	});
});
