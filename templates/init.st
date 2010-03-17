$base(content={
<h1>initialize</h1>
<form action="/init" method="post">
  <div class="field">
	<input type="text" name="name" id="name" value="$in.name$">
	<label for="name">name</label>
	<br />
  </div>
  <div class="field">
	<input type="text" name="description" id="description" value="$in.description$">
	<label for="description">description</label>
	<br />
  </div>
  <div class="field">
	<input type="text" name="website" id="website" value="$in.website$">
	<label for="website">website</label>
	<br />
  </div>
  <div class="field">
	<input type="text" name="bootstrap" id="bootstrap" value="$in.bootstrap$">
	<label for="bootstrap">bootstrap</label>
	<br />
  </div>
  <div class="buttons">
	<input type="submit" value="create repository" />
  </div>
</form>
})$
