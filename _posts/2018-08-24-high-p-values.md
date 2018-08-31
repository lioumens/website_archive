---
layout: post
title: "High p-values"
categories: [statistics]
tags: [p-values]
description: ""
comments: true
---

On the topic of hypothesis testing, I recently had a student ask why
statisticians say we "fail to reject" the null hypothesis when we have
non-significant p-values. Furthermore, why aren't higher p-values more evidence
for the null? After all, higher p-values do signify that the data they observed
are very consistent with the null hypothesis. 

I couldn't give a great answer on the spot, but I'll share the best analogy I can think of now.

Let's back up and define the hypothesis testing framework. To begin, I will
have two hypotheses, the null and alternative. The null hypothesis is some
claim that is being tested, normally the default. The alternative is the theory
of interest, normally either one-sided or two-sided when working with
population parameters. I will observe some sort of data, and calculate a
p-value is some measure of how "odd" this observed data given that the null
hypothesis is true. For example, my null hypothesis could be,

$$
H_0: \text{The average number of blue M&M's in a fun size pack is 3 } (\mu = 3) \\
H_A: \text{The average number of blue M&M's in a fun size pack is less than 3 } (\mu\leq = 3)
$$

This would be an example of a one sided hypothesis.

A low p-value in this case would indicate that we counted the blue M&M's in a
fun size pack and there were a statistically significant number of packs that
had less than 3 blue M&M's. A high p-value wouldmean that our observed data is
consistent with the null, and we would "fail to reject the null". If we ran two
experiements, and got two high p-values, .5 vs .99, we might be tempted to say
that there's stronger evidence that the average number of blue M&M's in a pack
is 3 from the second experiment than the first.

Here's another example that I think clarifies the difference. We could think of
the following set of hypotheses,

$$
H_0: \text{I don't like bananas} \\
H_A: \text{I like bananas}
$$

Say you saw me eating a banana for an afternoon snack. That would be some
pretty strong evidence that the null hypothesis is not true, and thus give a
very low p-value. Why would I eat the bananas if I didn't like them? Suppose
you saw me eating 2 bananas, or even 5 bananas. That data would be even MORE
evidence to suggest that the null hypothesis is not true. These observations
correspond to lower p-values. That's why we can compare low p-values to say
there's more evidence against the hypothesis.

Now suppose that you saw me drinking a smoothie with a little bit of banana
blended into the drink. In a smoothie, the banana isn't the predominant taste,
so I could still not like bananas but not mind the texture that the banana
added to the smoothie. Thus, since this observation is not necessarily
inconsistent with the null, it would perhaps correspond to a p-value of .5.

Finally, consider if you saw me eating a chocolate bar. This again, is
consistent with the null hypothesis and would perhaps give a p-value of .9.
However, because you saw me eating a chocolate bar, you would never say that
"Because I saw him eating a chocolate bar, that makes me absolutely positive
that he doesn't like bananas". This conclusion doesn't make any sense! The
chocolate bar had nothing to do with the banana, and doesn't tell me anything
in either direction of these hypotheses. However, if we interpret higher
p-values for more evidence for the null, this is effectively the nonsensical
conclusion we would be making. That's why statisticians are sticklers for the
verbiage "failing to reject the null hypothesis."

P-values have always been a somewhat tricky framework to fully understand
that's full of little nuances. It's no wonder why some scientific journals are
considering banning p-values entirely.

If you would like a better understanding of some of the pitfalls with the
p-value I would recommend reading through [A Dirty Dozen: Twelve P-Value
Misconceptions](http://www.perfendo.org/docs/BayesProbability/twelvePvaluemisconceptions.pdf).

P.S. For the record, I think bananas are okay...
