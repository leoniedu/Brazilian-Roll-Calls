import wordpresslib
content = open('post.txt','r').read()
title  = open('title.txt','r').read()
cfield = open('imagelink.txt','r').read()
user = 'admin'
passwd = 'e321109'
url = 'http://cluelessresearch.com/xmlrpc.php'
wp = wordpresslib.WordPressClient(url, user, passwd)
wp.selectBlog(0)
post = wordpresslib.WordPressPost()
post.title = title
post.description = content
post.categories = (wp.getCategoryIdFromName('Headline'),)
post.custom_fields = custom_fields
##idPost = 228
##idPost = wp.editPost(idPost, post,True)
idPost = wp.newPost(post, True)
## date can only be set while editing
## get data from R
post.date = '20010603T12:00:00'
editPost = wp.editPost(idPost, post,True)


##categories = [{'categoryId' : 'Headline', 'isPrimary' : 1}] 
## 30 is headline
## 31 is featured
## 34 is Votacoes
##categories = [{'categoryId' : 31},{'categoryId' : 5}] 
categories = [{'categoryId' : 34}] 

custom_fields = [{ "key" : "Image", "value" : cfield }]

blog_content = { 'title' : title, 'description' : content, 'custom_fields' : custom_fields,'dateCreated' : '2003 10 13' }

post_id = int(server.metaWeblog.newPost(blog_id, user, passwd, blog_content,0))

server.mt.setPostCategories(post_id, user, passwd, categories) # not work
server.mt.publishPost(post_id, user, passwd)
sys.stdout.write(str(post_id))
sys.stdout.flush()
