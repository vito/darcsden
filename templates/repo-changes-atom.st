<?xml version="1.0" encoding="utf-8"?>

<feed xmlns="http://www.w3.org/2005/Atom">
  <title>$user.uName$/$repo.name$ changes</title>
  <id>$baseurl$$user.uName$/$repo.name$/changes/atom/</id>
  <updated>2010-03-16T00:00:00Z</updated>
  <author>
    <name>$user.uName$</name>
    <uri>$baseurl$$user.uName$</uri>
  </author>
  <link href="$baseurl$$user.uName$/$repo.name$/changes/atom/" rel="self" />
  <link href="$baseurl$$user.uName$/$repo.name$/changes/" />

$patches:{ log |
  <entry>
    <title>$log.pName$</title>
    <id>$baseurl$$user.uName$/$repo.name$/patch/$log.pID$</id>
    <updated>$log.pDate$</updated>
    <author>
      <name>$log.pAuthor$</name>
    </author>
    <summary>$[log.pName,log.pLog];separator="
"$</summary>
    <link href="$baseurl$$user.uName$/$repo.name$/patch/$log.pID$" />
  </entry>
}$

</feed>
