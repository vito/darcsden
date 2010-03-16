$base(content={
<h1>log in</h1>
<form action="/login" class="login" method="post">
  <fieldset>
    <div class="field">
      <input type="text" name="name" value="$in.name$" />
      <label for="name">Name</label>
      <br />
    </div>
    <div class="field">
      <input type="password" name="password" />
      <label for="password">Password</label>
      <br />
    </div>
    <div class="buttons">
      <input type="submit" value="log me in" />
	  <br />
    </div>
  </fieldset>
</form>
})$
