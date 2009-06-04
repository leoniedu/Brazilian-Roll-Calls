import xmlrpclib

user = 'admin'
passwd = 'e321109'
server = xmlrpclib.ServerProxy('http://cluelessresearch.com/xmlrpc.php')

blog_id = 0
title = 'posting a page with python' 
content = 'test posting a new page'

blog_content = { 'title' : title, 'description' : content }
categories = [{'categoryId' : 'programming', 'isPrimary' : 1}] 

post_id = int(server.metaWeblog.newPost(blog_id, user, passwd, blog_content))
server.wp.editPage(1,86,user, passwd, blog_content, 1)
