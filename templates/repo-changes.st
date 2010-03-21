$repobase(
  extrahead={<link href="changes/atom" type="application/atom+xml" rel="alternate" title="$repo.name$ changes" />};
  content={
<h1>changes</h1>
<ul class="repo-log">
  $patches:{ log |
  <li>
    <h2>
      $if(log.pIsUser)$
      <a href="/$log.pAuthor$">$log.pAuthor$</a>
      $else$
      $log.pAuthor$
      $endif$
      :: <span class="relatize date">$log.pDate$</span>
    </h2>
    <p><a href="/$user.uName$/$repo.name$/patch/$log.pID$">$log.pName$</a></p>
    $if(log.pLog)$<p>$log.pLog;separator="
"$</p>$endif$
  </li>
  }$
</ul>
<ul class="pagination">
  $if(notFirst)$
  <li class="prev"><a href="/$user.uName$/$repo.name$/changes/page/$prevPage$">&lt;- prev</a></li>
  $endif$
  $if(notLast)$
  <li class="next"><a href="/$user.uName$/$repo.name$/changes/page/$nextPage$">next -&gt;</a></li>
  $endif$
  <li>page $page$ of $totalPages$</li>
</ul>
})$

