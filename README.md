Personal Website
===

This was created with [Jekyll](https://jekyllrb.com/), and is a modified theme from [dbtek/dbyll](https://github.com/dbtek/dbyll).


## Rakefile

There is a [rakefile](https://github.com/ruby/rake) in this repository (a makefile for ruby) to automate some basic tasks. The usage of the rakefile is as follows

`rake post title="POST TITLE" [date="TODAY"] [tags=[tag1,tag2]] [category="category"]`
  - Creates a new post in `_posts/` with today's date prepended in the title. The `yaml` header will automatically be created in the post with comments turned on by default as well

`rake page name="about.html`
  - Creates a new page with appropriate title.

`rake preview`
  - Alias for running `bundle exec jekyll preview`. The bundle exec is to run the command in the context of the current bundle context outlined by the `Gemfile`


## Directory Structure

* `_includes/`:  contains the template of default, post and page.
* `_posts/`: contains the published blog posts. Without dates means that it's a [draft](https://jekyllrb.com/docs/posts/).
* `_layouts/`: contains further information about how the posts should look.

## Troubleshooting

* commonmark gem
  * https://stackoverflow.com/questions/63729369/commonmarker-gem-cannot-be-installed-needed-for-jekyll-macos

* Liquid Syntax trying to process raw code chunks

> Liquid Exception: Liquid syntax error (line 211): Variable '{{` is short for this common pattern. The arguments need to be defused because otherwise R will try to evaluate the expression in its orginal environment. ```{r}' was not properly terminated with regexp: /\}\}/ in rmd/advanced/advanced.Rmd

  * https://stackoverflow.com/questions/52324134/getting-an-liquid-exception-liquid-syntax-error-while-using-jekyll
  
## R Markdown Files

## Quarto Files

This is a the first yaml block you've been using

``` yaml
title: "The Within-Between Model"
author: "Michael Liou"
format: 
  html:
    mainfont: Palitino
    code-fold: true
    theme: lumen
    linestretch: 1.2
    fontsize: 1em
    html-math-method: katex
execute:
  cache: true
```

