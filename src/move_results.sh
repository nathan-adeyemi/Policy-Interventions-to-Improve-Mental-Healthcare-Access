#!/bin/bash

move_results() {
    res_dir="$1"
    tmp_dir="$2"
    names=("${@:3}")

    # Create 'best_versions' directory if it doesn't exist
    if [ ! -d "$res_dir/best_versions" ]; then
        mkdir -p "$res_dir/best_versions"
    fi
    
    # Move files from tmp_dir to res_dir
    for file in "$tmp_dir"/*; do
        mv "$file" "$res_dir/"
    done

    # Copy directories from tmp_dir to best_versions
    for name in "${names[@]}"; do
        cp -r "$tmp_dir/$name" "$res_dir/best_versions/trainbale_${name}"
    done
}

move_results "$res_dir" "$tmp_dir" "${names[@]}"
