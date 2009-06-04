import xmlrpclib
import sys

content = open('post.txt','r').read()
title  = open('title.txt','r').read()
cfield = open('imagelink.txt','r').read()

user = 'admin'
passwd = 'e321109'
server = xmlrpclib.ServerProxy('http://cluelessresearch.com/xmlrpc.php')

blog_id = 0
# title = 'posting a new headline.  ' 
# content = 'what happens? '

##categories = [{'categoryId' : 'Headline', 'isPrimary' : 1}] 
## 30 is headline
## 31 is featured
## 34 is Votacoes
##categories = [{'categoryId' : 31},{'categoryId' : 5}] 
categories = [{'categoryId' : 34}] 

custom_fields = [{ "key" : "Image", "value" : cfield }]

blog_content = { 'title' : title, 'description' : content, 'custom_fields' : custom_fields }

post_id = int(server.metaWeblog.newPost(blog_id, user, passwd, blog_content,0))
server.mt.setPostCategories(post_id, user, passwd, categories) # not work
server.mt.publishPost(post_id, user, passwd)
sys.stdout.write(str(post_id))
sys.stdout.flush()
