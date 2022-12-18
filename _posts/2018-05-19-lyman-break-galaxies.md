---
layout: post
title: "Lyman Break Galaxies"
categories: [data science]
tags: [astronomy]
description: "Data science project: Searching for Lyman-Break Galaxies"
comments: true
---

This project was a data science exercise that was part of a class, and a collaboration with astronomer Dr. Christy Tremonti at the University of Madison, Wisconsin. 

## Introduction

First, I’ll add a disclaimer, that I am not an astronomer, but this is the
background of the project as I understand it. I’m more of a *very* amateur
astronomer that likes to look up at the night sky from time to time.

The Big Bang was an estimated 13.7 billion years ago, which is now considered
the “age of the universe”.[^universewiki] For context, Earth is estimated to be
about 4.6 billion years old, and the first signs of life are estimated to be
around 3.8 billion years ago. In order to study the universe in its earlier
stages, astronomers study star formation and galaxy formation. One of the
relatively easier galaxy classes to study are called Lyman-break and Lyman-α
galaxies. With some reasonable estimates, the light we observe from these
galaxies are about 12 billion years ago, pretty close to the beginning of the
universe![^firstgalaxies]

How do we estimate the age of the galaxy that we’re observing? After all, we
can really only point our telescopes at it and make observations. We can
estimate the age of the light that we see by the phenomenon of redshift. That
is, because our universe is expanding, the frequency of light that is emitted
from an object that is receding (other galaxy) will have a perceived stretched
wavelength. *Redshift* comes from how visible light will be “shifted” toward
the lower frequency end of the spectrum (as opposed to blueshifted). This is
also known as the Doppler effect.[^doppler] Thus, the more a wavelength a stretched, the
earlier that light was emitted in the timeline of the universe. By measuring
the wavelength of the observed light, we can estimate that light emitted from
Lyman-break galaxies is from when the galaxy is about ~1 billion years old.[^firstgalaxies]
Studying these galaxies gives us a window into the evolution of our universe.
Strategies for identifying these galaxies are not foolproof though.

There are a few distinctive characterizations of Lyman-α galaxies, but to
understand them, we should first understand how they came to be known as
Lyman-α galaxies. The name Lyman comes from the American physicist Dr. Theodore
Lyman who discovered that when hydrogen gas cools, it emits ultraviolet rays,
which can be viewed as emissions in a spectroscope. The first of these
emissions has a wavelength of 121.6 nanometers (nm). Relatedly, another
physicist Johannes Rydberg showed a mathematical formula to predict the
wavelength of these emissions depending on the initial principal quantum number
(n) or how much potential energy the electron has.[^rydberg]

\\[
\frac{1}{\lambda} = R_H \left(1 - \frac{1}{n^2}\right)
\\]

![Lyman Series]({{"assets/media/lymanseries.svg" | absolute_url }})

With this formula, it was shown mathematically that for a hydrogen atom, the
lowest possible wavelength emission from an excited hydrogen atom is 91.2 nm
(in the ultraviolet range). The wavelengths of possible light emissions for
hydrogen is known as the *Lyman Series*.[^lymanseries] Since galaxies are predominantly
hydrogen gas, they will emit light in this range, all above 91.2 nm. In order
to identify Lyman-break galaxies, astronomers can image the galaxies at many
filters, and look for the signature drop in emission intensity at the
Lyman-break. There are several complications though, for example, we know that
the Lyman limit is at 91.2 nm, but since these galaxies are redshifted, we
cannot be certain of the exact wavelength this break will occur. In addition,
newly formed stars within the galaxies will have emissions outside of the Lyman
Series, and surrounding neutral gases can absorb these emissions. Having
quality image data is also fairly difficult to acquire, as sufficiently deep
imaging with a good signal to noise ratio can be expensive. Finally, there are
many contaminants that can look very similar to Lyman-break galaxies, such as
Active Galactic Nuclei, or quasars, at lower red shifts and cool galactic
stars. These objects can create a quick drop in flux density, which can easily
be mistaken for the Lyman-break dropout.[^firstgalaxies]


# The Data Problem

We were provided a spectrograph of a true Lyman-break Galaxy named
“cb58”[^cb58] and the spectrographs of nearly 2.5 million other spectrographs
from the [Sloan Digital Sky Survey (SDSS)](http://www.sdss.org/). The basic idea was to create some
sort of “similarity” metric to our known Lyman break galaxy for the 2.5 million
other spectrographs. Since we can process these spectrographs in parallel, we
took advantage of the [Center for High Throughput Computing at UW-Madison](http://chtc.cs.wisc.edu/), which
boasts roughly 30,000 CPU cores to support research. With massive computing
capabilities, we are able to power through our search in a matter of a few
hours.

<div>
    <a href="https://plot.ly/~lioumens/1/?share_key=vZWZrKEPuXtdi4YmQj5W2Q" target="_blank" title="blog_cb58" style="display: block; text-align: center;"><img src="https://plot.ly/~lioumens/1.png?share_key=vZWZrKEPuXtdi4YmQj5W2Q" alt="blog_cb58" style="max-width: 100%;width: 600px;"  width="600" onerror="this.onerror=null;this.src='https://plot.ly/404.png';" /></a>
    <script data-plotly="lioumens:1" sharekey-plotly="vZWZrKEPuXtdi4YmQj5W2Q" src="https://plot.ly/embed.js" async></script>
</div>

In order to identify other Lyman-break galaxies from the other spectra, I tried
a simple similarity metric which is just a rolling window, pointwise Euclidean
distance to our known galaxy. Because the spectrum we have may be redshifted an
arbitrary amount depending on how far the galaxy is, and the spectra data may
be of different lengths, we have to consider “sliding” the spectrum against
each other and finding the minimum among all the shifted similarity scores. We
then compare similarity scores across all spectrum and report the most similar
among all the spectra.

Here was one of our top matches. It turns out that this match that we found is
another known Lyman-break galaxy named “8 o’clock Arc”.[^8oclock] Looking at both of the
spectra together, with one super imposed on the other, we can see how similar
they are. We scale the intensity in this graph because we cannot expect the
absolute intensities between the two galaxies to be the same, but we can look
at the general shape by noting the reltaive intensity. Note the distinctive
Lyman-alpha emissions at 1216Å.

<div>
    <a href="https://plot.ly/~lioumens/3/?share_key=lLAKXvfxELMJZokki5GsDw" target="_blank" title="blog_cb58_with_8oclock" style="display: block; text-align: center;"><img src="https://plot.ly/~lioumens/3.png?share_key=lLAKXvfxELMJZokki5GsDw" alt="blog_cb58_with_8oclock" style="max-width: 100%;width: 600px;"  width="600" onerror="this.onerror=null;this.src='https://plot.ly/404.png';" /></a>
    <script data-plotly="lioumens:3" sharekey-plotly="lLAKXvfxELMJZokki5GsDw" src="https://plot.ly/embed.js" async></script>
</div>

Just to see how similar the contaminants can look, here is another convincing
match that our algorithm found that was identified as a quasar, or active
galactic nuclei. In general, we can rule out the quasars by identifying broad
1216, Ci IV 1550 and CIII 1909 angstrom emissions, but this one may be harder
to tell.

<div>
    <a href="https://plot.ly/~lioumens/5/?share_key=VGngrdf4YunhLO3P1xWT1x" target="_blank" title="blog_cb58_with_quasar" style="display: block; text-align: center;"><img src="https://plot.ly/~lioumens/5.png?share_key=VGngrdf4YunhLO3P1xWT1x" alt="blog_cb58_with_quasar" style="max-width: 100%;width: 600px;"  width="600" onerror="this.onerror=null;this.src='https://plot.ly/404.png';" /></a>
    <script data-plotly="lioumens:5" sharekey-plotly="VGngrdf4YunhLO3P1xWT1x" src="https://plot.ly/embed.js" async></script>
</div>

## Final Words

There is a lot of work to be done to correctly filter out all of the
contaminants, but the basic algorithm already gives a lot of convincing
matches. Even the contaminants are not always clearly identified as stars or
quasars. However, with 2.5 million spectra to look at, a simple algorithm can
at least filter these down to a reaonable set of a few thousand. 

If you’re interested in reading more about active research into these Lyman
break galaxies, there is a wealth of information out there. To start, here are
some of the resources that I used to understand some of the background
information. [^1] [^2] [^3]

## References

[^firstgalaxies]:[Observing the First Galaxies](https://arxiv.org/pdf/1205.1543.pdf)
[^lymanseries]:[Lyman Series](https://en.wikipedia.org/wiki/Lyman_series)
[^8oclock]: [The 8 o'clock Arc: A Serendipitous Discovery of a Strongly Lensed Lyman Break Galaxy in the SDSS DR4 Imaging Data](https://arxiv.org/abs/astro-ph/0611138)
[^cb58]:[Insight into Lyman Break Galaxies](https://arxiv.org/pdf/astro-ph/9908007.pdf)
[^universewiki]: [Timeline of the Universe Wikipedia](https://en.wikipedia.org/wiki/Timeline_of_the_formation_of_the_Universe)
[^doppler]: [Doppler Effect Wikipedia](https://en.wikipedia.org/wiki/Doppler_effect)
[^rydberg]: [Rydberg Formula Wikipedia](https://en.wikipedia.org/wiki/Rydberg_formula)

[^1]:[Handout on Lyman break Galaxies](http://www.star.bris.ac.uk/stanway/lecture1_handout.pdf)
[^2]:[Exploring Lyman-Alpha and Lyman-Break Galaxies](http://hubblesite.org/hubble_discoveries/science_year_in_review/pdf/2012/exploring_lyman_alpha_and_lyman_break_galaxies.pdf)
[^3]:[Star Formation Wikipedia](https://en.wikipedia.org/wiki/Star_formation)

