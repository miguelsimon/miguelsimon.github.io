---
# You don't need to edit this file, it's empty on purpose.
# Edit theme's home layout instead if you wanna make some changes
# See: https://jekyllrb.com/docs/themes/#overriding-theme-defaults
title: "A blog about interesting problems"
layout: default
---

<h1>{{ page.title }}</h1>

## Posts:

<ul class="posts">

	{% for post in site.posts reversed %}
        <li><span>{{ post.date | date_to_string }}</span> <a href="{{ post.url }}" title="{{ post.title }}">{{ post.title }}</a></li>
	{% endfor %}
</ul>

