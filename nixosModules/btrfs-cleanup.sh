#!/bin/sh

if [ -z "$1" ]; then
    echo "No path specified" >&2
    exit 1
fi

if [ "${1#/}" == "$1" ]; then
    echo "The given path isn't absolute: $1" >&2
    exit 1
fi

is_subvolume() {
    [ "$(stat -f --format="%T" "$1")" == "btrfs" ] && [ "$(stat --format="%i" "$1")" == "256" ]
}

if ! is_subvolume "$1"; then
    echo "$1 isn't a subvolume!" >&2
    exit 1
fi

echo "Deleting $1" >&2

output=$(btrfs subvolume delete "$1" 2>&1) && exit 0
if ! echo "$output" | grep -q "Directory not empty"; then
    echo "Failed to delete $1 before recursion" >&2
    echo "$output" >&2
    exit  1
fi

output=$(btrfs property set "$1" ro false 2>&1) || {
    echo "Failed to mark $1 as read-write" >&2
    echo "$output" >&2
    exit 1
}

find "$1" -mindepth 1 -xdev -inum 256 -exec "$0" "{}" \;

output=$(btrfs subvolume delete "$1" 2>&1) || {
    echo "Failed to delete $1" >&2
    echo "$output" >&2
    exit 1
}

exit 0
