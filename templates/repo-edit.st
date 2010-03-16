$repobase(content={
<h1>edit repository</h1>
<form action="/$repo.owner$/$repo.name$/edit" method="post">
  <fieldset>
    <div class="field">
      <input type="text" name="name" id="name" value="$repo.name$" />
      <label for="name">name</label>
      <br />
    </div>
    <div class="field">
      <input type="text" name="description" id="description" value="$repo.description$" />
      <label for="description">description</label>
      <br />
    </div>
    <div class="field">
      <input type="text" name="website" id="website" value="$repo.website$" />
      <label for="website">website</label>
      <br />
    </div>
	<div class="field">
	  <input type="text" name="add-members" id="add-members" />
	  <label for="add-members">add members</label>
	  <span class="note">comma separated</span>
	</div>
	<div class="field">
	  <ul>
		$members:{ member |
		<li><input type="checkbox" name="remove-$member$" /> <a href="/$member$">$member$</a></li>
		}$
	  </ul>
	  <label for="remove-members">remove members</label>
	  <br />
	</div>
    <div class="buttons">
      <input type="submit" value="update repository" />
	  <br />
    </div>
  </fieldset>
</form>
})$
