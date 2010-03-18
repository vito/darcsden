$base(
  title="browse :: darcsden";
  crumb="browse";
  content={
<h1>all repositories</h1>
<ul class="repo-list">
  $repos:{ repo |
  <li>
	<div class="title"><a class="repo-name" href="/$repo.owner$/$repo.name$">$repo.name$</a> :: <a href="/$repo.owner$">$repo.owner$</a></div>
	<p class="repo-desc">
	  $repo.description$
	</p>
  </li>
  }$
</ul>
<ul class="pagination">
  $if(notFirst)$
  <li class="prev"><a href="/browse/page/$prevPage$">&lt;- prev</a></li>
  $endif$
  $if(notLast)$
  <li class="next"><a href="/browse/page/$nextPage$">next -&gt;</a></li>
  $endif$
  <li>page $page$ of $totalPages$</li>
</ul>
})$
