$repobase(
  title={$user.uName$'s $repo.name$ :: darcsden};
  crumb={
$path:{ d |
-&gt; <a href="$d.iURL$">$d.iName$</a>
}$
};
  content={
$if(files)$
<h1>files</h1>
<ul class="repo-files">
  $if(up)$
  <li class="up"><a href="$up$">..</a></li>
  $endif$
  $files:{ file |
  <li class="$if(file.iIsDirectory)$directory$else$file$endif$"><a href="$file.iURL$">$file.iName$</a></li>
  }$
</ul>
$else$
<h1>nothing here yet!</h1>
<p class="repo-empty">push your code to <code>$user.uName$@darcsden.com:$repo.name$</code> to get started</p>
$endif$

$if(readme)$
<h1>readme</h1>
<div class="readme">
  $readme:noescape()$
</div>
$endif$
})$
