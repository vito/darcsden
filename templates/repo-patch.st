$repobase(content={
<h1>patch</h1>
<ul class="repo-log">
  <li>
	<h2>
	  $if(log.pIsUser)$
	  <a href="/$log.pAuthor$">$log.pAuthor$</a>
	  $else$
	  $log.pAuthor$
	  $endif$
	  :: <span class="relatize date">$log.pDate$</span>
	</h2>
	<p>$log.pName$</p>
	$if(log.pLog)$<p>$log.pLog;separator="
"$</p>$endif$
  </li>
</ul>

$if(summary)$
<h1>summary</h1>
<ul class="patch-summary">
  $summary:{ change |
  $if(change.preference)$
  <li class="summary-preference">changed "$change.preference$" preference from "$change.from$" to "$change.to$"</li>
  $elseif(change.modified)$
  <li class="summary-modified"><a href="#$change.modified$">$change.modified$</a></li>
  $elseif(change.removed)$
  <li class="summary-removed">$change.removed$</li>
  $elseif(change.added)$
  <li class="summary-added"><a href="/$repo.owner$/$repo.name$/browse/$change.added$">$change.added$</a></li>
  $elseif(change.replaced)$
  <li class="summary-replace"><a href="/$repo.owner$/$repo.name$/browse/$change.replaced$">$change.replaced$</a> replaced <code>$change.from$</code> with <code>$change.to$</code></a></li>
  $endif$
  }$
</ul>
$endif$

$if(changes)$
<h1>changes</h1>
<ul class="patch-changes">
  $changes:{ change |
  <li id="$change.cfName$">
	<h2><a href="/$repo.owner$/$repo.name$/browse/$change.cfName$">$change.cfName$</a> :: <span class="line">line $change.cfType.fchLine$</span></h2>
	<div class="removed">$change.cfType.fchRemove:noescape()$</div>
	<div class="added">$change.cfType.fchAdd:noescape()$</div>
  </li>
  }$
</ul>
$endif$
})$
