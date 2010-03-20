$repobase(
crumb="-> forks";
content={
<form action="/$repo.owner$/$repo.name$/merge" class="subtle" method="post">
  <fieldset>
    $forks:{ fork |
    <h1><a href="/$fork.repo.owner$/$fork.repo.name$">$fork.repo.name$</a> :: $fork.repo.owner$</h1>
    <table class="fork-log">
      $fork.changes:{ log |
      <tr>
        $if(isAdmin)$
        <td class="merge">
          <input type="checkbox" name="merge:$fork.repo.owner$:$fork.repo.name$:$log.pID$"  />
        </td>
        $endif$
        <td class="name">
          <p><a href="/$fork.repo.owner$/$fork.repo.name$/patch/$log.pID$">$log.pName$</a></p>
        </td>
        <td class="author">
          $if(log.pIsUser)$
          <a href="/$log.pAuthor$">$log.pAuthor$</a>
          $else$
          $log.pAuthor$
          $endif$
        </td>
        <td class="date">
          <span class="relatize date">$log.pDate$</span>
        </td>
      </tr>
      }$
    </table>
    }$
    $if(isAdmin)$
    <br />
    <div class="buttons">
      <input type="submit" value="merge selected" />
    </div>
    $endif$
  </fieldset>
</form>
})$
