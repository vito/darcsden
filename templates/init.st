$base(content={
<h1>initialize</h1>
<form action="/init" method="post">
  <div class="field">
	<input type="text" name="name" value="$in.name$">
	<label for="name">name</label>
	<br />
  </div>
  <div class="field">
	<input type="text" name="description" value="$in.description$">
	<label for="description">description</label>
	<br />
  </div>
  <div class="field">
	<input type="text" name="website" value="$in.website$">
	<label for="website">website</label>
	<br />
  </div>
  <div class="buttons">
	<input type="submit" value="create repository" />
  </div>
</form>
})$