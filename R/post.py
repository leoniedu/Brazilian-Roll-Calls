content = open('post.txt','r').read()
title  = open('title.txt','r').read()
date =  open('date.txt','r').read()
cats =  open('category.txt','r').read()
post_id = open('postid.txt','r').read()
custom_fields = [{ "key" : "Image", "value" : open('imagelink.txt','r').read() }]
tags = open('tags.txt','r').read()
title = 'again'
content = 'try'
date = xmlrpclib.DateTime('20090615T12:12:34')
##cats = "MPV"
cats = ['Data', 'Uncategorized']
post_id = '461'
custom_fields = [{ "key" : "Image", "value" :"hello" }]
tags = 'tag1 tag2'
post_id = 'NA'

user = 'admin'
passwd = 'e321109'
url = 'http://cluelessresearch.com/xmlrpc.php'


##1. create post with xmlrpclin, since it can include custom fields
import xmlrpclib
import sys
import datetime
import time

blog_content = { 'title' : title, 'description' : content, 'custom_fields' : custom_fields,'dateCreated' : date, 'categories' : cats}
server = xmlrpclib.ServerProxy(url)

blog_id = 0
if post_id=='NA' :
    newpost=True
    post_id = int(server.metaWeblog.newPost(blog_id, user, passwd, blog_content,1))
else :
    newpost=False
    post_id=int(post_id)
    ## find image custom field
    for i in server.metaWeblog.getPost(post_id,user,passwd)['custom_fields'] :
        if i['key']=='Image' :
            imageid=i['id']
            ## put something here if there is no image custom field
    custom_fields = [{"id" : imageid, "key" : "Image", "value" : cfield }]    
    blog_content['custom_fields'] = custom_fields
    #This always deletes whatever was in the post!
    server.metaWeblog.editPost(post_id, user, passwd, blog_content,1)
    

sys.stdout.write(str(post_id))
sys.stdout.flush()
