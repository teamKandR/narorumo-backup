#!/bin/bash

## NB: This wants to be sourced from your .bashrc, not run directly.

function goto() {
  cd "`goto.py $*`"
}
