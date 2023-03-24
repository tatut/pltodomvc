#!/bin/sh

cd $GITHUB_WORKSPACE/cypress-todomvc
npm install
npx cypress run
