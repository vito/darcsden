$repobase(
  crumb={
$path:{ d |
-&gt; <a href="$d.iURL$">$d.iName$</a>
}$};
  content={
<h1><a href="$file.iURL$">$file.iName$</a></h1>
<div class="code">
  $blob:noescape()$
</div>
})$