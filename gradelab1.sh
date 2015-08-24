#!/usr/bin/env bash

SBT_PROJECT="project grader"
SBT_ARGS="test-only Lab1Grading"
sbt "$SBT_PROJECT" "$SBT_ARGS"
