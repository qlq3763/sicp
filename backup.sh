#!/bin/bash
src_dir=./
dest_dir=/media/My\ Book/backup
backup_date=`date +"%Y-%m-%d"`
pack_file_name=sicp-$backup_date.tar.bz2

echo "starting ...."

tar -jcv -f $pack_file_name $src_dir/*

mv $src_dir/$pack_file_name "$dest_dir"

echo "backup date:" $backup_date
echo "backup" $src_dir "to" $dest_dir "with packed file name:" $pack_file_name
echo "done!"

exit 0
