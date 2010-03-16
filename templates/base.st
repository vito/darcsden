<!DOCTYPE html>
<html>
  <head>
	<title>$title; null="darcsden"$</title>
	<link rel="stylesheet" href="/public/css/main.css" type="text/css" media="screen" />
	<link rel="stylesheet" href="/public/css/hk-kate.css" />
	<script src="/public/js/jquery.js" type="text/javascript"></script>
	<script src="/public/js/relatize.js" type="text/javascript"></script>
	<script src="/public/js/main.js" type="text/javascript"></script>
  </head>
  <body>
	<ul class="links nav">
	  $if(visitor)$
	  <li class="settings"><a href="/settings">settings</a></li>
	  <li class="logout"><a href="/logout">log out</a></li>
	  $else$
	  <li class="login"><a href="/login">log in</a></li>
	  <li class="register"><a href="/register">register</a></li>
	  $endif$
	</ul>

	<h1 class="head">
	  <a href="/">darcsden</a>
	  ::
	  $crumb; null="index"$
	</h1>

	$notifications:{ n |
	<div class="notification $n.type$">$n.message$</div>
	}$

	$content$

	<div class="clear"></div>
	<div class="footer">
	  <a class="tg" href="http://toogeneric.com/">too generic</a>
	  <p>darcsden &copy; alex suraci 2010</p>
	  <p>follow me on <a href="http://twitter.com/alexsuraci">twitter</a></p>
	</div>
  </body>
</html>
