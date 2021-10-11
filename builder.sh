#!/bin/sh
# shellcheck shell=bash

set -exu

. "${scriptPath:-}"
echo -n "${result:-}" > "${out:-}"
