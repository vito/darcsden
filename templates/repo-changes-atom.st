<?xml version="1.0" encoding="utf-8"?>

<feed xmlns="http://www.w3.org/2005/Atom">
  <title>$user.uName$/$repo.name$ changes</title>
  <id>http://darcsden.com/$user.uName$/$repo.name$/changes/atom/</id>
  <updated>2010-03-16T00:00:00Z</updated>
  <author>
    <name>$user.uFullName$</name>
    <uri>http://darcsden.com/$user.uName$</uri>
  </author>
  <link href="http://darcsden.com/$user.uName$/$repo.name$/changes/atom/" rel="self" />
  <link href="http://darcsden.com/$user.uName$/$repo.name$/changes/" />

$patches:{ log |
  <entry>
    <title>$log.pName$</title>
    <id>$log.pID$</id>
    <updated>$log.pDate$</updated>
    <author>
      <name>$log.pAuthor$</name>
    </author>
    <summary>$log.pLog$</summary>
    $if(log.pIsUser)$
    <link href="http://darcsden.com/$log.pAuthor$/$repo.name$/patch/$log.pID$" />
    $endif$
  </entry>
}$

</feed>
