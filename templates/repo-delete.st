$repobase(content={
<h1>are you sure you want to delete this repository?</h1>
<p class="blurb">this action cannot be undone.</div>

<form action="/$repo.owner$/$repo.name$/delete" method="post">
  <fieldset>
	<div class="cancel-buttons">
	  <input type="submit" value="yes, delete this repository" /> or <a href="/$repo.owner$/$repo.name$" class="cancel">cancel</a>
	  <br />
	</div>
  </fieldset>
</form>
})$
