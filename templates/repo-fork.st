$repobase(content={
<h1>you already have a repository named "$name$"</h1>
<p class="blurb">please create an alternative name:</p>

<form action="/$repo.owner$/$repo.name$/fork-as" method="post">
  <fieldset>
	<div class="field">
	  <input type="text" name="name" id="name" value="$name$-" />
	  <label for="name">name</label>
	  <br />
	</div>
	<div class="buttons">
	  <input type="submit" value="fork" />
	  <br />
	</div>
  </fieldset>
</form>
})$