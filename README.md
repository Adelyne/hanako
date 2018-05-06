# Hanako

[![Build Status](https://travis-ci.com/Adelyne/hanako.svg?branch=master)](https://travis-ci.com/Adelyne/hanako)

A resilient imageboard.

Work in progress, there is nothing besides tests to run.

## REST API

In `/path/to/:var/resource`, `:var` designates a variable portion of the URL.

Boards:

 - `GET /boards` + query string for display modification
 - `POST /boards` (create)
 - `GET /board/:id` (single board)
 - `DELETE /board/:id`
 - `PATCH /board/:id` (edit)
 - `POST /board/:id` (edit)

Threads:

 - `GET /threads` + query string for display modification
 - `GET /threads/:board` (threads of a board)
 - `POST /threads/:board` (create)
 - `GET /thread/:board/:threadid` (single thread)
 - `PATCH /thread/:board/:threadid` (edit)
 - `POST /thread/:board/:threadid` (edit)
 - `DELETE /thread/:board/:threadid`

Posts:

 - `GET /posts` (all posts) + query string for display modification and filtering
 - `GET /posts/:board/:threadid` (all posts in a thread) + query string for display modification
 - `POST /posts/:board/:threadid` (create a post in a thread)
 - `GET /posts/reply/:postid` (`posts` in `reply` to a post with `:postid`)
 - `GET /post/:board/:id` (single post)
 - `PATCH /post/:board/:id` (edit)
 - `POST /post/:board/:id` (edit)
 - `DELETE /post/:board/:id`

## Test

    $ rebar3 dialyzer
    $ rebar3 eunit
    $ rebar3 ct
