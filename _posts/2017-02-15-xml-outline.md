---
layout: post
title: "Generate XML Outline in Python"
categories: []
tags: [python]
description: "Here's a short example of a short recursive function to help parse through XML files."
comments: true
---

When parsing through xml files, it's often helpful to have a general outline of the elements. Unfortunately, most python packages for parsing xml do not have a built-in function to view the general structure without viewing the raw xml file. Here is an example of a short recursive function in python to create such an outline.

The example file we use is [simple.xml](https://www.w3schools.com/xml/simple.xml).

```python
import xml.etree.ElementTree as ET
import textwrap

def outline_XML(root, level=0, indent="  "):
    for child in root:
        # shorten requires Python 3.4+
        childtext = textwrap.shorten(child.text, width=70) if child.text else ""
        print("{0}{1}({2}): {3}".format(indent*level,
                                        child.tag,
                                        ",".join(child.keys()),
                                        childtext))
        outline(child, level+1)

tree = ET.parse("simple.xml")
root = tree.getroot()

outline_XML(root)
```

The output will look like this:

```
food(): 
  name(): Belgian Waffles
  price(): $5.95
  description(): Two of our famous Belgian Waffles [...]
  calories(): 650
food(): 
  name(): Strawberry Belgian Waffles
  price(): $7.95
  description(): Light Belgian waffles covered with [...]
  calories(): 900
food(): 
  name(): Berry-Berry Belgian Waffles
  price(): $8.95
  description(): Light Belgian waffles covered with [...]
  calories(): 900
food(): 
  name(): French Toast
  price(): $4.50
  description(): Thick slices made from our [...]
  calories(): 600
food(): 
  name(): Homestyle Breakfast
  price(): $6.95
  description(): Two eggs, bacon or sausage, toast, [...]
  calories(): 950
```

Note that these XML tags did not have any attributes and that's why there is nothing between the parentheses.
