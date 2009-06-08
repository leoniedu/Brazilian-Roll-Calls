import xmlrpclib
import sys
import datetime
import time

content = open('post.txt','r').read()
title  = open('title.txt','r').read()
date =  xmlrpclib.DateTime(open('date.txt','r').read())
cats =  open('category.txt','r').read()
cats = cats.split()

post_id = open('postid.txt','r').read()
custom_fields = [{ "key" : "Image", "value" : open('imagelink.txt','r').read() }]
tags = open('tags.txt','r').read()
# title = 'again'
# content = 'try'
# date = xmlrpclib.DateTime('20090615T12:12:34')
# ##cats = "MPV"
# cats = ['Data', 'Uncategorized']
# post_id = '461'
# custom_fields = [{ "key" : "Image", "value" :"hello" }]
# tags = 'tag1 tag2'
# post_id = 'NA'

user = 'admin'
passwd = '&A9V$nunJAFS'
url = 'http://congressoaberto.eduardoleoni.com/xmlrpc.php'


##1. create post with xmlrpclin, since it can include custom fields

##cats = [7,8]
##cats = ['test','PEC']
blog_content = { 'title' : title, 'description' : content, 'custom_fields' : custom_fields,'dateCreated' : date, 'categories' : cats , 'mt_keywords' : tags}
server = xmlrpclib.ServerProxy(url)

blog_id = 0
if post_id=='NA' :
    newpost=True
    post_id = int(server.metaWeblog.newPost(blog_id, user, passwd, blog_content,1))
else :
    newpost=False
    post_id=int(post_id)
    ## find image custom field
    j=0
    ji=-1
    bc = server.metaWeblog.getPost(post_id,user,passwd)
    for i in bc['custom_fields'] :
        if i['key']=='Image' :
            ji=j
            imageid=i['id']
        j=j+1
    if  ji > -1 :
        bc['custom_fields'][ji]['value'] = open('imagelink.txt','r').read()
    else :
        bc['custom_fields'] = custom_fields ##not working
    bc['title']=title
    bc['description']=content
    bc['dateCreated']=date
    bc['categories']=cats
    bc['keywords']=tags
    server.metaWeblog.editPost(post_id, user, passwd, bc,1)
    
sys.stdout.write(str(post_id))
sys.stdout.flush()











