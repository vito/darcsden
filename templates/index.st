$base(content={
$if(visitor)$
<h1><a href="/init">create a repository -></a></h1>

$if(hasRepositories)$
<br />
<h1>your repositories</h1>
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
$endif$
$else$
<p class="blurb">darcsden is a place to share your <a href="http://darcs.net/">darcs</a> projects and collaborate with others.</p>
<p class="blurb">it is currently undergoing very active development, and is not yet finished, although it does seem to be pretty stable. i'll keep downtimes as brief as possible, but they will be there (usually when I'm fixing things and need to re-launch it, which only takes a second).</p>
<p class="blurb">if you want to help out or just poke around an example repository, feel free to take a gander at <a href="/alex/darcsden">darcsden's source code</a>!</p>
$endif$
})$

