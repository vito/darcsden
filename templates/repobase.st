$base(crumb={
<a href="/$repo.owner$">$repo.owner$</a>
-&gt; <a href="/$repo.owner$/$repo.name$">$repo.name$</a>
$crumb$};
content={
<ul class="links">
  $if(isAdmin)$
  <li class="edit"><a href="/$repo.owner$/$repo.name$/edit">edit</a></li>
  <li class="delete"><a href="/$repo.owner$/$repo.name$/delete">delete</a></li>
  $endif$
  <li class="fork"><a href="/$repo.owner$/$repo.name$/fork">fork</a></li>
  <li class="changes"><a href="/$repo.owner$/$repo.name$/changes">changes</a></li>
</ul>

<p class="repo-desc">
  $repo.description$$if(repo.website)$ &mdash; <a href="$repo.website$">$repo.website$</a>$endif$
</p>

$content$
})$
