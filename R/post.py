
content = open('post.txt','r').read()
title  = open('title.txt','r').read()
date =  open('date.txt','r').read()
cats =  open('category.txt','r').read()
# date formate '20090610T12:12:34'
# title = 'again'
# content = 'try'
cfield = open('imagelink.txt','r').read()
custom_fields = [{ "key" : "Image", "value" : cfield }]
user = 'admin'
passwd = 'e321109'
url = 'http://cluelessresearch.com/xmlrpc.php'


##1. create post with xmlrpclin, since it can include custom fields
import xmlrpclib
import sys
blog_content = { 'title' : title, 'description' : content, 'custom_fields' : custom_fields}
server = xmlrpclib.ServerProxy(url)
blog_id = 0
post_id = int(server.metaWeblog.newPost(blog_id, user, passwd, blog_content,0))



## get categories
## should change this, may be hard code the categories
import wordpresslib
wp = wordpresslib.WordPressClient(url, user, passwd)
wp.selectBlog(0)
post = wordpresslib.WordPressPost()
post.title = title
post.description = content
post.categories = (wp.getCategoryIdFromName(cats),)
## date can only be set while editing
## get data from R
post.date = date
editPost = wp.editPost(post_id, post,True)

sys.stdout.write(str(post_id))
sys.stdout.flush()
