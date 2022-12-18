---
layout: post
title: "Flex Boxes in CSS"
categories: [web]
tags: [css]
description: "It's worth learning."
comments: true
---

# Introduction

Flex boxes are an absolute godsend. Flexboxes are a new specification of CSS, that excels at "one-way layout" (as opposed to the two way CSS grid layout).

In brief flex-boxes give you more flexibility over the layout of a website, by specifying one main axis (horizontal or vertical), then gives you control over how your html elements are spaced along that axis. For example, if you want all the elements centered, or equally spaced around, the layout is very intuitive. Great introductions to the flexbox (and web programing) are given here.

* [Interneting is hard](https://internetingishard.com/html-and-css/flexbox/)
* [CSS Tricks](https://css-tricks.com/snippets/css/a-guide-to-flexbox/)
* [Game with frogs](https://flexboxfroggy.com/)


# Brief Rant

CSS is a nightmare when it comes to understanding the nuances of floats, clears, inline, blocks, inline-blocks. At least at first when unfamiliar with CSS. How you imagine the layout of the page simply doesn't translate well to the CSS commands used create that layout. Add in a flurry of different screen sizes and require that the website or web app needs to be responsive, and have some specific vertical layout and the CSS will be even more of a mess. In short, learning flexboxes will replace a lot of previous frustration making layouts in CSS.

## Example

The introduction websites above give a great overview of what flex boxes are capable of, as well as great images with the pictures. Here's a working example to get you started. Note that most of the CSS applies to the container for the items, not the items themselves.

<p data-height="257" data-theme-id="0" data-slug-hash="zaQwNL" data-default-tab="html,result" data-user="lioumens" data-embed-version="2" data-pen-title="flexbox_example" class="codepen">See the Pen <a href="https://codepen.io/lioumens/pen/zaQwNL/">flexbox_example</a> by Michael Liou (<a href="https://codepen.io/lioumens">@lioumens</a>) on <a href="https://codepen.io">CodePen</a>.</p>
<script async src="https://static.codepen.io/assets/embed/ei.js"></script>
