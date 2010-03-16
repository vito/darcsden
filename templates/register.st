$base(content={
<h1>sign up</h1>
<form action="/register" method="post">
  <fieldset>
    <div class="field">
      <input type="text" name="name" value="$in.name$" />
      <label for="name">name</label>
      <br />
    </div>
    <div class="field">
      <input type="text" name="email" value="$in.email$"/>
      <label for="email">email</label>
      <br />
    </div>
    <div class="field">
      <input type="password" name="password1" />
      <label for="password">password</label>
      <br />
    </div>
    <div class="field">
      <input type="password" name="password2" />
      <label for="password">(repeat)</label>
      <br />
    </div>
    <div class="buttons">
      <input type="submit" value="sign me up" />
	  <br />
    </div>
  </fieldset>
</form>
})$
