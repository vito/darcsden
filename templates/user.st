$base(
  title={$user.uName$ :: darcsden};
  crumb={<a href="/$user.uName$">$user.uName$</a>};
  content={
<h1>$user.uName$'s repositories</h1>
$if(hasRepositories)$
<ul class="user-repos">
  $repositories:{ repo |
  <li>
	<a href="$repo.owner$/$repo.name$">$repo.name$</a>
	$if(repo.description)$
	&mdash; $repo.description$
	$endif$
	$if(repo.website)$
	(<a href="$repo.website$">$repo.website$</a>)
	$endif$
  </li>
  }$
</ul>
$else$
<p class="blurb">nothing to see here! move along!</p>
$endif$
})$
