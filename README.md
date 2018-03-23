
### Data Mining Project - Crowdfunding User Contribution

Within the project I try and put in place various models which, based on the features of a user,
predicts as to where he or she will contribute to a recommended project. 

To view this project in its html page please click the link below:
http://rpubs.com/ajoseph/371399


### Data Description

The dataset contains a total of 25 column, off which 23 are user features. One column belongs 
to the target variable and the other one is the user_id which can be ignored.

The 23 variables are as follows:

* same_owner : a boolean which expresses the fact that the user has already supported a project 
headed by the same project leader as the recommended project.
* owner_friend : a boolean which expresses whether the head of the recommended project was one 
the user had supported before.
* nb_friends : an integer value expressing the number of Facebook friends of the user who 
has supported the recommend project.
* amount_friends : a real number expressing investments made for the recommended project by 
the user's Facebook friends.
* nb_copledgers : a real number expressing contirbuting users who have contributed to similar 
projects as the user in the past.
* amount_copledgers : a real number expressing investments  by users who have invested in similar 
projects as given user.
* desc_score_mean : a real number value which express similarity between projects proposed and 
projects the user has already participated in.
* dist : an integer representing physical distance between user and the recommended project.

The other variables concern the different categories of projects on the platform and
symbolize the fact that the user has, or not, already supported projects of the same category
as the project recommended. This inclination towards a category is represented by a number between 0
and 1, 1 means complete inclination for the category and 0 means that the user never
participated in a project of this category or that the proposed project is not in this
category. The list of categories is as follows:
music
- live-performance
- journalism
- book-and-publishing
- design-and-innovation
- event
- film-and-video
- style
- photography
- social
- web-and-tech
- education
- art
- adventure-and-sport
- food
- ecology

 

