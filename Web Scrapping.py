# -*- coding: utf-8 -*-
"""
Created on Mon Sep  2 18:04:02 2019

@author: juan.urrutia
"""

#First we need to import the request library
import requests
page = requests.get("http://dataquestio.github.io/web-scraping-pages/simple.html")
page

#We can print the content of the web page using the content function
page.content


#And then you can use the BeautyfulSoup to parse the downloaded content of the web page
from bs4 import BeautifulSoup
soup = BeautifulSoup(page.content, 'html.parser')
soup

#Now we can use the prettify method in our BeautyfulSoup object to see the html code in a really better undertanding way than before
print(soup.prettify())

#With the previous code you can see the nested tags. 
#Now is time to start digging into the tags to extract the paragraph that we want.
#We can use the children method to get into the nested tags one level at a time. 
#Note that the children method returns a list with the nested tags so we need to use the list function to see the nested tags.
soup.children

list(soup.children)

#We found 3 elements using the children method. 
#We can see the type of the elements using the type function
[type(item) for item in list(soup.children)]
#•The first element is a Doctype object which contain information about the type of the document
#•The second element is a NavegableString which is text found in the code
#•And finally the last element is a Tag object. This is the object what we are looking for because this object contains the other tags and the information that we need.

#We can now select the html tag saving the last element of the previous list into a new variable
html = list(soup.children)[2]
html

#Each one of the returned items by the children method are also a BeautyfulSoup object so we can use the children method with those returned items
#So here we go again !!
list(html.children)


#Now we have 5 elements. Three of them are just text. 
#The other two elements are the Head tag and the Body tag which contain the p tag that we want to extract (We are almost there!)
#So we have to repeat the same as before
body = list(html.children)[3]
body

#And now our last effort to get the data
list(body.children)

p = list(body.children)[1]
p

#We finally have the p tag in our pocket so to get the content of the p tag we need 
#to use the get_text method in our variable to extract all of the text from it
p.get_text()

#We did it !!!

#We can now learn a better way to do the same
#Finding all instances of a tag at once
#All the previous steps was needed to make you understand how to navigate through a web page 
#and get the data that you want, but as you can see it take us a lot of code and effort to get the data.

#There is another way to do the same. 
#We can use the find_all method to get all the instances of a tag in just one shot.
soup = BeautifulSoup(page.content, 'html.parser')
soup.find_all('p')

#Note that find_all method returns a list of elements so we need to use an index to extract the text
soup.find_all('p')[0].get_text()

#In case you want to get just the first element in the html code of a particular tag, you can use the 'find' method
soup.find('p')
#Note that the previous output is not a list so you should use the get_text method without using index

#Searching for tags using class and id attributes

#Class and Id are two attributes that can be included in html code.
#Class attribute allow you to categorize one or more elements into groups
#Id attribute allow you to identify an element. A certain Id can be used by just one element and an element can have just one Id

#We can use this attibutes when we are scraping to get particular elements in our searchs so we can filter effectively

#To understand this we are going to use the following web page: http://dataquestio.github.io/web-scraping-pages/ids_and_classes.html
page = requests.get("http://dataquestio.github.io/web-scraping-pages/ids_and_classes.html")
soup = BeautifulSoup(page.content, 'html.parser')
soup

soup.find_all('p', class_='outer-text')

#You can also search for elemetns using the Id attribute
soup.find_all(id="first")

#CSS selectors
#When you are searching for elements you can also do it using CSS selectors. Remember that CSS language allows developers to specify html tags to style. Here you have some examples:
#•p a : finds all "a" tags inside of a "p" tag
#•body p a : finds all "a" tags inside of a "p" tag inside of a "body" tag
#•html body : finds all "body" tags inside of an "html" tag
#•p.outer-text : finds all "p" tags with a class of "outer-text"
#•p#first : finds all "p" tags with an id of "first"
#•body p.outer-text : finds any "p" tags with a class of "outer-text" inside of a "body" tag

#The great thing is that BeautyfulSoup objects support searching a web page via CSS selectors. 
#The only difference is that you have to use the select method.
#Note that select method returns a list of elements as equal as find and find_all methods
soup.select("div p")

#Parsel
#Parsel is a python library for extracting data from XML/HTML text using CSS or XPath selectors.
#In this case we are going to use the following html code

html = u'''
<ul>
    <li><a href="http://blog.scrapinghub.com">Blog</a></li>
...
    <li><a href="https://www.scrapinghub.com">Scrapinghub</a></li>
...
    <li class="external"><a href="http://www.scrapy.org">Scrapy</a></li>
</ul>
'''

#Now we have to import the Parsel library, load it into a Parsel Selector and extract links with an XPath expression
import parsel
sel = parsel.Selector(html)
sel.xpath("//a/@href").extract()

#One of the best features of Parsel is the ability to chain selectors !!
sel.css('li.external').xpath('./a/@href').extract()

#You can also iterate through the results of the .css() and .xpath() methods since each element will be another selector
for li in sel.css('ul li'):
    print(li.xpath('./a/@href').extract_first())
    
#Thank You!