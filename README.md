# [Kris Brown's personal website](https://web.stanford.edu/~ksb/)

This code is for both generating a website as well as taking math notes (taken in an org-mode file) and transforming them into web pages, ideally in the style of [Project Crazy Project](https://web.archive.org/web/20140327002205/http://crazyproject.wordpress.com/aadf/#df-1).

The goal of sharing my solutions to textbook problems online is not only to help others but also to get feedback! Self-studying topics like math is difficult, and there are likely many errors to be found. My hope is that the comment feature would be in part used for this.

The project is generated by running `stack build; stack exec web build; stack exec web deploy`.
