$base(content={
<h1>settings</h1>
<form class="settings" action="/settings" method="post">
  <fieldset>
    <div class="field">
      <input type="text" name="name" value="$user.uFullName$" />
      <label for="name">full name</label>
      <br />
    </div>
    <div class="field">
      <input type="text" name="website" value="$user.uWebsite$" />
      <label for="website">website</label>
      <br />
    </div>
    <div class="field">
      <textarea rows="10"name="pubkeys" id="pubkeys">$pubkeys$</textarea>
      <label for="pubkeys">pubkeys</label>
	  <span class="note">one per line</span>
    </div>
    <div class="buttons">
      <input type="submit" value="update settings" />
	  <br />
    </div>
  </fieldset>
</form>
})$